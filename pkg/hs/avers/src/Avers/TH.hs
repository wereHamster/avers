{-# LANGUAGE TemplateHaskell #-}

module Avers.TH
  ( deriveJSON
  , deriveJSONOptions
  , variantOptions
  , defaultVariantOptions

  , deriveEncoding
  , deriveRecordEncoding

  , FromJSON(..)
  , ToJSON(..)
  ) where


import           Language.Haskell.TH

import           Control.Applicative
import           Control.Monad

import           Data.Char

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Aeson.Types as A

import           Text.Inflections

import qualified Database.RethinkDB as R
import           Database.RethinkDB (toDatum, FromDatum(..))
import           Database.RethinkDB.TH

import           Prelude


dropPrefix :: String -> String -> String
dropPrefix prefix x = toLower (head rest) : tail rest
  where rest = drop (length prefix) x


-- FIXME: Should dasherize and drop the prefix from the constructor as well!
deriveJSONOptions :: String -> Options
deriveJSONOptions prefix = defaultOptions
    { fieldLabelModifier     = dropPrefix prefix
    , constructorTagModifier = map toLower
    }

dasherizeTag :: String -> String
dasherizeTag x = case parseCamelCase [] (T.pack x) of
    Left  err -> error $ show err
    Right res -> map toLower $ T.unpack $ dasherize res

variantOptions :: String -> String -> String -> Options
variantOptions tagField contentsField prefix = defaultOptions
    { constructorTagModifier = dasherizeTag . dropPrefix prefix
    , sumEncoding            = TaggedObject tagField contentsField
    }

defaultVariantOptions :: String -> Options
defaultVariantOptions = variantOptions "type" "content"


deriveEncoding :: Options -> Name -> Q [Dec]
deriveEncoding opts name =
    liftM2 (++)
        (deriveJSON opts name)
        (deriveDatum opts name)


deriveEncodingWithDefaults :: Options -> Name -> [(String, Q Exp)] -> Q [Dec]
deriveEncodingWithDefaults options typeN defaults =
    liftM2 (++)
        (deriveJSONWithDefaults options typeN defaults)
        (deriveDatumWithDefaults options typeN defaults)


deriveRecordEncoding :: Name -> String -> [(String, Q Exp)] -> Q [Dec]
deriveRecordEncoding typeN prefix defaults =
    deriveEncodingWithDefaults (deriveJSONOptions prefix) typeN defaults



------------------------------------------------------------------------------
-- JSON

deriveJSONWithDefaults :: Options -> Name -> [(String, Q Exp)] -> Q [Dec]
deriveJSONWithDefaults options typeN defaults = liftM2 (++)
    (deriveToJSON options typeN)
    (deriveFromJSONWithDefaults options typeN defaults)

deriveFromJSONWithDefaults :: Options -> Name -> [(String, Q Exp)] -> Q [Dec]
deriveFromJSONWithDefaults options typeN defaults = do
    xN                        <- newName "x"
    (tyVarsBndrs, fieldNames) <- getTyInfo typeN
    parseJsonE                <- mkParseJSON options typeN
    insertDefaultsE           <- mkInsertDefaults defaults
    modifyObjectE             <- [|modifyObject|]
    let tyVars      = tyVarBndrNames tyVarsBndrs
        fieldNames' = map (fieldLabelModifier options . nameBase) fieldNames

    forM_ defaults $ \(name, _) ->
        when (name `notElem` fieldNames') $ fail $
            "Avers.TH.deriveFromJSONWithDefaults: default " ++
            "specified for " ++ show name ++ " but this field does not exist."

    let icxt = map (\tv -> AppT (ConT ''FromJSON) (VarT tv)) tyVars
    return
        [ InstanceD Nothing icxt
            (AppT fromJsonT (mkAppTys (typeT : map VarT tyVars)))
            [ FunD 'parseJSON
                [ Clause [VarP xN]
                    (NormalB (AppE parseJsonE (mkApps
                        [modifyObjectE, insertDefaultsE, VarE xN])))
                    []
                ]
            ]
        ]
  where
    typeT     = ConT typeN
    fromJsonT = ConT ''FromJSON

modifyObject :: (A.Object -> A.Object) -> A.Value -> A.Value
modifyObject f (A.Object o) = A.Object (f o)
modifyObject _ v            = v
{-# INLINE modifyObject #-}

mkInsertDefaults :: [(String, Q Exp)] -> Q Exp
mkInsertDefaults defaults = do
    yN    <- newName "y"
    bodyE <- foldM insertDefault (VarE yN) defaults
    return $ LamE [VarP yN] bodyE
  where
    flipE       = VarE 'flip
    constE      = VarE 'const
    insertWithE = VarE 'HM.insertWith
    toJsonE     = VarE 'toJSON

    insertDefault hmE (field, expQ) = do
        textE    <- [|T.pack field|]
        defaultE <- expQ
        return $ mkApps [insertWithE, AppE flipE constE,
                textE, AppE toJsonE defaultE, hmE]


------------------------------------------------------------------------------
-- Datum

deriveDatumWithDefaults :: Options -> Name -> [(String, Q Exp)] -> Q [Dec]
deriveDatumWithDefaults options typeN defaults = liftM2 (++)
    (deriveToDatum options typeN)
    (deriveFromDatumWithDefaults options typeN defaults)

deriveFromDatumWithDefaults :: Options -> Name -> [(String, Q Exp)] -> Q [Dec]
deriveFromDatumWithDefaults options typeN defaults = do
    xN                        <- newName "x"
    (tyVarsBndrs, fieldNames) <- getTyInfo typeN
    parseJsonE                <- mkParseDatum options typeN
    insertDefaultsE           <- mkInsertDatumDefaults defaults
    modifyObjectE             <- [|modifyDatumObject|]
    let tyVars      = tyVarBndrNames tyVarsBndrs
        fieldNames' = map (fieldLabelModifier options . nameBase) fieldNames

    forM_ defaults $ \(name, _) ->
        when (name `notElem` fieldNames') $ fail $
            "Avers.TH.deriveFromDatumWithDefaults: default " ++
            "specified for " ++ show name ++ " but this field does not exist."

    let icxt = map (\tv -> AppT (ConT ''FromDatum) (VarT tv)) tyVars
    return
        [ InstanceD Nothing icxt
            (AppT fromDatumT (mkAppTys (typeT : map VarT tyVars)))
            [ FunD 'parseDatum
                [ Clause [VarP xN]
                    (NormalB (AppE parseJsonE (mkApps
                        [modifyObjectE, insertDefaultsE, VarE xN])))
                    []
                ]
            ]
        ]
  where
    typeT      = ConT typeN
    fromDatumT = ConT ''FromDatum

modifyDatumObject :: (R.Object -> R.Object) -> R.Datum -> R.Datum
modifyDatumObject f (R.Object o) = R.Object (f o)
modifyDatumObject _ v            = v
{-# INLINE modifyDatumObject #-}

mkInsertDatumDefaults :: [(String, Q Exp)] -> Q Exp
mkInsertDatumDefaults defaults = do
    yN    <- newName "y"
    bodyE <- foldM insertDefault (VarE yN) defaults
    return $ LamE [VarP yN] bodyE
  where
    flipE       = VarE 'flip
    constE      = VarE 'const
    insertWithE = VarE 'HM.insertWith
    toJsonE     = VarE 'toDatum

    insertDefault hmE (field, expQ) = do
        textE    <- [|T.pack field|]
        defaultE <- expQ
        return $ mkApps [insertWithE, AppE flipE constE,
                textE, AppE toJsonE defaultE, hmE]



mkApps :: [Exp] -> Exp
mkApps = foldl1 AppE

mkAppTys :: [Type] -> Type
mkAppTys = foldl1 AppT

getTyInfo :: Name -> Q ([TyVarBndr], [Name])
getTyInfo typeN = do
    info <- reify typeN
    case info of
        TyConI (DataD _ _ tvbs _ cons _)   -> pure (tvbs, getFieldNames =<< cons)
        TyConI (NewtypeD _ _ tvbs _ con _) -> pure (tvbs, getFieldNames con)
        _                                -> fail $
            "Avers.TH.getTyInfo: only simple data/newtype " ++
            "constructors are supported."

tyVarBndrNames :: [TyVarBndr] -> [Name]
tyVarBndrNames = map $ \tvb -> case tvb of
    PlainTV  n   -> n
    KindedTV n _ -> n

getFieldNames :: Con -> [Name]
getFieldNames (NormalC _ _)         = []
getFieldNames (RecC _ fields)       = [name | (name, _, _) <- fields]
getFieldNames (InfixC _ _ _)        = []
getFieldNames (ForallC _ _ con)     = getFieldNames con
getFieldNames (GadtC _ _ _)         = []
getFieldNames (RecGadtC _ fields _) = [name | (name, _, _) <- fields]
