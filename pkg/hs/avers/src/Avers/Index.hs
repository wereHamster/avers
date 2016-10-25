{-# LANGUAGE GADTs #-}

module Avers.Index where


import           Data.Text (Text)
import qualified Database.RethinkDB as R


data Index a = Index
  { indexName       :: Text
  , indexExpression :: R.Exp R.Object -> R.Exp a
  }


data SomeIndex where
     SomeIndex :: (R.IsDatum a) => Index a -> SomeIndex
