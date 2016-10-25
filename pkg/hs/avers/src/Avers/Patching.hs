{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Avers.Patching
    ( applyOperation
    , opOT
    , rebaseOperation
    , resolvePathIn
    ) where


import           Data.Monoid

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as M

import           Data.Aeson
import           Data.Aeson.Types

import           Avers.Types (Patch(..), Path(..), Operation(..), PatchM, PatchError(..))

import           Control.Monad


-- | Apply the given op on the value. Can throw an exception if the operation
--   is invalid.
applyOperation :: Value -> Operation -> PatchM Value
applyOperation value Set{..}
    | opPath == "", Just val <- opValue = return val
    | otherwise = changeObject value opPath $ \key ->
        maybe (M.delete key) (M.insert key) opValue

applyOperation value Splice{..} = changeArray value opPath $ \a -> do

    -- Check if the indices are within the allowed range.
    when (V.length a < opIndex + opRemove) $
        Left $ UnknownPatchError $ mconcat
            [ "Index out of range ("
            , T.pack (show $ V.length a)
            , ","
            , T.pack (show opIndex)
            , ","
            , T.pack (show opRemove)
            , ")"
            ]

    -- The existing array and the elements we want to insert must match
    -- structurally (have the same type). Furthermore, if the array consists
    -- of objects, each object is required to have an "id" field.
    unless (isStructurallyEquivalent opInsert a) $
        Left $ UnknownPatchError "Array doesn't match structure"

    return $ V.take opIndex a V.++ V.fromList opInsert V.++ V.drop (opIndex + opRemove) a

  where
    isStructurallyEquivalent :: [Value] -> V.Vector Value -> Bool
    isStructurallyEquivalent a b = strings a b || validObjects a b

    strings      a b = all isString    a && V.all isString    b
    validObjects a b = all hasObjectId a && V.all hasObjectId b


isString :: Value -> Bool
isString (String _) = True
isString _          = False

hasObjectId :: Value -> Bool
hasObjectId (Object o) = M.member "id" o
hasObjectId _          = False

pathElements :: Path -> [Text]
pathElements = T.split ('.' ==) . unPath

changeObject :: Value -> Path -> (Text -> Object -> Object) -> PatchM Value
changeObject value path f = changeObjectAt value (init (pathElements path)) $ \x ->
    case x of
        Object o -> Right $ Object $ f (last (pathElements path)) o
        _        -> Left $ UnknownPatchError "Can not change a non-object"

changeArray :: Value -> Path -> (Array -> PatchM Array) -> PatchM Value
changeArray value path f = changeObjectAt value (pathElements path) $ \x ->
    case x of
        Array a -> fmap Array $ f a
        _       -> Left $ UnknownPatchError "Can not change a non-array"


changeObjectAt :: Value -> [Text] -> (Value -> PatchM Value) -> PatchM Value
changeObjectAt container [] f = f container

changeObjectAt (Object o) (x:xs) f =
    case parse (const $ o .: x) o of
        Error   _ -> Left $ UnknownPatchError $ "Key '" <> T.pack (show x) <> "' does not exist inside the object"
        Success a -> do
            new <- changeObjectAt a xs f
            return $ Object $ M.insert x new o


changeObjectAt (Array a) (x:xs) f =
    case V.findIndex (matchObjectId x) a of
        Nothing    -> Left $ UnknownPatchError $ "Can not find item with id " <> T.pack (show x) <> " in the array"
        Just index -> do
            new <- changeObjectAt (a V.! index) xs f
            return $ Array $ a V.// [(index, new)]

changeObjectAt _ _ _ = Left $ UnknownPatchError "Can not descend into primitive values"


matchObjectId :: Text -> Value -> Bool
matchObjectId itemId (Object o) = Just (String itemId) == M.lookup "id" o
matchObjectId _      _          = False



-- | Resolve the path in the object.
resolvePathIn :: Path -> Value -> Maybe Value
resolvePathIn path = go (pathElements path)
  where
    go []     value      = Just value
    go [""]   value      = Just value

    go (x:xs) (Object o) =
        case parse (const $ o .: x) o of
            Error   _ -> Nothing
            Success a -> go xs a

    go (x:xs) (Array a)  =
        maybe Nothing (go xs) $ V.find (matchObjectId x) a

    go _      _          = Nothing



-- Set (foo)        -> Set (foo)        = ok
-- Set (foo)        -> Set (foo.bar)    = drop
-- Set (foo.bar)    -> Set (foo)        = ok
-- Set (foo)        -> Set (bar)        = ok
--
-- Set (foo)        -> Splice (foo)     = drop
-- Set (foo)        -> Splice (foo.bar) = drop
-- Set (foo.bar)    -> Splice (foo)     = ok
-- Set (foo)        -> Splice (bar)     = ok
--
-- Splice (foo)     -> Set (foo)        = ok
-- Splice (foo)     -> Set (foo.bar)    = ok if foo.bar exists
-- Splice (foo.bar) -> Set (foo)        = ok
-- Splice (foo)     -> Set (bar)        = ok
--
-- Splice (foo)     -> Splice (foo)     = drop -- todo: ok (adjust)
-- Splice (foo)     -> Splice (foo.bar) = ok if foo.bar exists
-- Splice (foo.bar) -> Splice (foo)     = ok
-- Splice (foo)     -> Splice (bar)     = ok

opOT :: Value -> Operation -> Operation -> Maybe Operation
opOT content base op

    -- Duplicate ops are dropped.
    | base == op = Nothing

    -- If neither is a prefix of the other (they touch distinct parts of the
    -- object) then it's safe to accept the op.
    | not ((opPath base `isPrefixOf` opPath op) || (opPath op `isPrefixOf` opPath base)) =
        Just op

    | otherwise = case base of
        Set{..}    -> setOT opPath
        Splice{..} -> spliceOT opPath

  where
    setOT path = case op of
        Set{..} -- Set -> Set
            | path == opPath             -> Just op
            | path `isPrefixOf` opPath   -> Nothing
            | otherwise                  -> Just op

        Splice{..} -- Set -> Splice
            | path == opPath             -> Nothing
            | path `isPrefixOf` opPath   -> Nothing
            | otherwise                  -> Just op

    spliceOT path = case op of
        Set{..} -- Splice -> Set
            | path == opPath             -> Just op
            | path `isPrefixOf` opPath   -> onlyIfPresent opPath
            | otherwise                  -> Just op

        Splice{..} -- Splice -> Splice
            | path == opPath             -> spliceOnSplice base op
            | path `isPrefixOf` opPath   -> onlyIfPresent opPath
            | otherwise                  -> Nothing

    onlyIfPresent path = case resolvePathIn path content of
        Nothing -> Nothing
        Just _  -> Just op

    (Path a) `isPrefixOf` (Path b) = a `T.isPrefixOf` b

    -- Both ops are 'Splice' on the same path.
    spliceOnSplice op1 op2
        | opIndex op1 + opRemove op1 <= opIndex op2
            = Just $ op2 { opIndex = opIndex op2 + (length $ opInsert op1) - opRemove op2 }

        | opIndex op2 + opRemove op2 < opIndex op1
            = Just op2

        | otherwise = Nothing



-- | Given an 'Operation' which was created against a particular 'Value'
-- (content), rebase it on top of patches which were created against the very
-- same content in parallel.
--
-- This function assumes that the patches apply cleanly to the content.
-- Failure to do so results in a fatal error.

rebaseOperation :: Value -> Operation -> [Patch] -> Maybe Operation
rebaseOperation _       op []     = Just op
rebaseOperation content op (x:xs) = case applyOperation content (patchOperation x) of
    Left e -> error $ "Unexpected failure: " ++ (show e)
    Right newContent -> case opOT newContent (patchOperation x) op of
        Nothing  -> Nothing
        Just op' -> rebaseOperation newContent op' xs
