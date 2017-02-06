{-# LANGUAGE OverloadedStrings #-}

module EvalSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "eval" $ do
    describe "success" $ do
      it "foo" $ do
        True `shouldBe` True