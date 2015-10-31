{-# LANGUAGE OverloadedStrings #-}

module ParseSpec where

import Test.Hspec
import Recon.Type
import Recon.Parse

spec :: Spec
spec = do
  describe "var" $ do
    describe "success" $ do
      it "0" $ do
        let
          script = "0"
          term   = Var (ValueVarName 0)
        parse script `shouldBe` Right term

  describe "zero" $ do
    describe "success" $ do
      it "zero" $ do
        let
          script = "zero"
          term   = Zero
        parse script `shouldBe` Right term

  describe "true" $ do
    describe "success" $ do
      it "true" $ do
        let
          script = "true"
          term   = TTrue
        parse script `shouldBe` Right term

  describe "false" $ do
    describe "success" $ do
      it "false" $ do
        let
          script = "false"
          term   = TFalse
        parse script `shouldBe` Right term

  describe "succ" $ do
    describe "success" $ do
      it "succ zero" $ do
        let
          script = "succ zero"
          term   = Succ Zero
        parse script `shouldBe` Right term

  describe "pred" $ do
    describe "success" $ do
      it "pred zero" $ do
        let
          script = "pred zero"
          term   = Pred Zero
        parse script `shouldBe` Right term

  describe "iszero" $ do
    describe "success" $ do
      it "iszero zero" $ do
        let
          script = "iszero zero"
          term   = IsZero Zero
        parse script `shouldBe` Right term

  describe "if" $ do
    describe "success" $ do
      it "if true then true else true" $ do
        let
          script = "if true then true else true"
          term   = If TTrue TTrue TTrue
        parse script `shouldBe` Right term

  describe "abs" $ do
    describe "success" $ do
      it "λ0. true" $ do
        let
          script = "λ0. true"
          term   = Abs (ValueVarName 0) TTrue
        parse script `shouldBe` Right term

  describe "let" $ do
    describe "success" $ do
      it "let 0 = true in true" $ do
        let
          script = "let 0 = true in true"
          term   = Let (ValueVarName 0) TTrue TTrue
        parse script `shouldBe` Right term

  describe "paren" $ do
    describe "success" $ do
      it "(true)" $ do
        let
          script = "(true)"
          term   = TTrue
        parse script `shouldBe` Right term

  describe "app" $ do
    describe "success" $ do
      it "true true" $ do
        let
          script = "true true"
          term   = App TTrue TTrue
        parse script `shouldBe` Right term
