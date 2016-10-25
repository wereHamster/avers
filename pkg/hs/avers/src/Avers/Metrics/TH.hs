{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Avers.Metrics.TH where


import           Control.Applicative

import           Data.Char
import           Data.List

import           Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as TH

import           System.FilePath            ((</>), dropFileName)

import           Prelude


toLabels :: [String] -> [[String]]
toLabels = go [] []
  where
    go :: [(Int, String)] -> [[String]] -> [String] -> [[String]]
    go _ res [] = res
    go ctx res (x:xs) = case compare prefixLength indent of
        EQ -> if null ctx
            then go [(0, label)] (res ++ [[label]]) xs
            else go ctx (res ++ [init (map snd ctx) ++ [label]]) xs
        GT -> go (ctx ++ [(prefixLength - indent, label)]) (res ++ [(map snd ctx) ++ [label]]) xs
        LT -> go (newCtx ++ [(prefixLength - newCtxIndent, label)]) (res ++ [map snd newCtx ++ [label]] ) xs

      where
        indent = sum $ map fst ctx
        prefixLength = length $ takeWhile (==' ') x
        label = dropWhile (==' ') x

        newCtx = init (pop (indent - prefixLength) ctx)
        newCtxIndent = sum $ map fst newCtx

        pop n cx
            | n <= 0 = cx
            | otherwise = case reverse cx of
                [] -> error "pop empty list"
                (cn, _):rest -> pop (n - cn) (reverse rest)


toMetrics :: [[String]] -> [[String]]
toMetrics [] = []
toMetrics (x:[]) = [x]
toMetrics (x : (y : rest)) = case compare (length y) (length x) of
    EQ -> [x] ++ toMetrics (y:rest)
    GT -> toMetrics (y:rest)
    LT -> [x] ++ toMetrics (y:rest)


mkMeasurements :: Q [Dec]
mkMeasurements = do
    filePath <- dropFileName . TH.loc_filename <$> TH.qLocation
    src <- runIO $ do
        body <- readFile (filePath </> "Measurements.txt")
        return $ filter (not . null) $ lines body

    let labels = toLabels src
    let metrics = toMetrics labels

    return
        [ DataD [] (mkName "Measurement") [] Nothing (map genCon metrics) []

        , SigD (mkName "measurementLabels") (AppT (AppT ArrowT (ConT (mkName "Measurement"))) (AppT ListT $ AppT ListT (ConT ''Char)))
        , FunD (mkName "measurementLabels")
            (map toClause metrics)
        ]

  where
    toName :: [String] -> Name
    toName labels = (mkName $ "M_" ++ (intercalate "_" labels))

    genCon :: [String] -> Con
    genCon labels =
        NormalC (toName labels) []

    toClause :: [String] -> Clause
    toClause labels =
        Clause [(ConP (toName labels) [])] (NormalB $ ListE $ map (LitE . StringL) labels) []
