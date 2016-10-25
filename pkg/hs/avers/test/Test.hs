{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec
import           Control.Exception (evaluate)

import           Data.Aeson          as A
import           Data.Aeson.Types
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HMS

import           Avers
import           Avers.Types (PatchM)
import           Avers.Patching

isFailure :: PatchM a -> Bool
isFailure (Left  _) = True
isFailure (Right _) = False

isSuccess :: PatchM a -> Bool
isSuccess (Left  _) = False
isSuccess (Right _) = True

main :: IO ()
main = hspec spec

objA :: A.Value
objA = A.Object $ HMS.fromList [("id", A.String "foo"),("bar", A.String "baz")]

spec :: Spec
spec = parallel $ do

    describe "resolvePathIn" $ do
        it "should return the value if the path is empty" $ do
            resolvePathIn "" Null `shouldBe` Just Null

        it "should return Nothing if the value is not a container" $ do
            resolvePathIn "x" Null        `shouldBe` Nothing
            resolvePathIn "x" (String "") `shouldBe` Nothing
            resolvePathIn "x" (Number 1)  `shouldBe` Nothing
            resolvePathIn "x" (Bool True) `shouldBe` Nothing

        it "should return Nothing if the path can't be resolved" $ do
            resolvePathIn "foo" emptyObject `shouldBe` Nothing

        it "should return object field if the path points into an object" $ do
            resolvePathIn "bar" objA `shouldBe` Just (A.String "baz")

        it "should pick the item from an array which has the given id" $ do
            resolvePathIn "foo.bar" (Array $ V.fromList []) `shouldBe` Nothing
            resolvePathIn "foo.bar" (Array $ V.fromList [emptyObject]) `shouldBe` Nothing
            resolvePathIn "foo.bar" (Array $ V.fromList [objA]) `shouldBe` Just (A.String "baz")


    describe "applyOperation" $ do
        it "should fail if it can't descend to the target value" $ do
            applyOperation emptyObject (Set "foo.bar.baz" Nothing) `shouldSatisfy` isFailure
            applyOperation emptyArray  (Set "foo.bar.baz" Nothing) `shouldSatisfy` isFailure

            applyOperation emptyObject (Splice "foo.bar.baz" 0 0 []) `shouldSatisfy` isFailure
            applyOperation emptyArray  (Splice "foo.bar.baz" 0 0 []) `shouldSatisfy` isFailure

        describe "Splice" $ do
            it "should handle primitive arrays" $ do
                let Right obj = applyOperation emptyObject (Set "array" (Just emptyArray))
                applyOperation obj (Splice "array" 0 0 ["elem"]) `shouldSatisfy` isSuccess

            it "should fail if the array doesn't match structure" $ do
                let Right obj = applyOperation emptyObject (Set "array" (Just $ Array $ V.singleton emptyObject))
                applyOperation obj (Splice "array" 0 0 ["elem"]) `shouldSatisfy` isFailure

            it "should fail if the array indices are out of range" $ do
                let Right obj = applyOperation emptyObject (Set "array" (Just emptyArray))
                applyOperation obj (Splice "array" 0 1 ["elem"]) `shouldSatisfy` isFailure

        describe "Set" $ do
            it "should replace the whole object if path is the root" $ do
                let (Right a) = applyOperation emptyObject $ Set "" (Just objA)
                a `shouldBe` objA
            it "should create a new key if the key doesn't exist yet" $ do
                let (Right a) = applyOperation emptyObject $ Set "foo" (Just Null)
                resolvePathIn "foo" a `shouldBe` Just Null
