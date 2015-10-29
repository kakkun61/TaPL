{-# LANGUAGE OverloadedStrings #-}

module ReconSpec where

import Test.Hspec
import Recon
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Either
import Text.Parsec

spec :: Spec
spec = do
  describe "CT-Var'" $ do
    describe "success" $ do
      it "x : Nat ⊦ x : Nat | ∅ {}" $ do
        let
          ctx   = Context $ M.singleton (ValueVarName 0) Nat
          term  = Var $ ValueVarName 0
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = Nat
          cons  = S.empty
          seed' = TypeVarNameSeed $ TypeVarName 0
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

      it "x : Bool ⊦ x : Bool | ∅ {}" $ do
        let
          ctx   = Context $ M.singleton (ValueVarName 0) TBool
          term = Var $ ValueVarName 0
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = TBool
          cons  = S.empty
          seed' = TypeVarNameSeed $ TypeVarName 0
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

      it "x : X ⊦ x : X | ∅ {}" $ do
        let
          ctx = Context $ M.singleton (ValueVarName 0) (TypeVar $ TypeVarName 0)
          term = Var $ ValueVarName 0
          seed  = TypeVarNameSeed $ TypeVarName 1
          typ   = TypeVar $ TypeVarName 0
          cons  = S.empty
          seed' = TypeVarNameSeed $ TypeVarName 1
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

      it "x : ∀X. X ⊦ x : S | {S} {}" $ do
        let
          ctx   = Context $ M.singleton (ValueVarName 0) (Scheme (S.singleton $ TypeVarName 0) (TypeVar $ TypeVarName 0))
          term  = Var $ ValueVarName 0
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = TypeVar $ TypeVarName 0
          cons  = S.empty
          seed' = TypeVarNameSeed $ TypeVarName 1
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

      it "x : ∀X Y. X ⊦ x : S1 | {S1, S2} {}" $ do
        let
          ctx   = Context $ M.singleton (ValueVarName 0) (Scheme (S.fromList $ map TypeVarName [0, 1]) (TypeVar $ TypeVarName 0))
          term  = Var $ ValueVarName 0
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = TypeVar $ TypeVarName 0
          cons  = S.empty
          seed' = TypeVarNameSeed $ TypeVarName 2
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

      it "x : ∀X Y. X → Y ⊦ x : S1 → S2 | {S1, S2} {}" $ do
        let
          ctx   = Context $ M.singleton (ValueVarName 0) (Scheme (S.fromList $ map TypeVarName [0, 1]) (Arrow (TypeVar $ TypeVarName 0) (TypeVar $ TypeVarName 1)))
          term  = Var $ ValueVarName 0
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = Arrow (TypeVar $ TypeVarName 0) (TypeVar $ TypeVarName 1)
          cons  = S.empty
          seed' = TypeVarNameSeed $ TypeVarName 2
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

    describe "failure" $ do
      it "∅ ⊦ x ⇒ fail" $ do
        let
          ctx  = Context $ M.empty
          term = Var $ ValueVarName 0
          seed = TypeVarNameSeed $ TypeVarName 0
        evalState (runExceptT $ ctype ctx term) seed `shouldSatisfy` isLeft

  describe "CT-AbsInf" $ do
    describe "success" $ do
      it "∅ ⊦ λx. x : S → S | {S} {}" $ do
        let
          ctx   = Context $ M.empty
          term  = Abs (ValueVarName 0) (Var $ ValueVarName 0)
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = Arrow (TypeVar $ TypeVarName 0) (TypeVar $ TypeVarName 0)
          cons  = S.empty
          seed' = TypeVarNameSeed $ TypeVarName 1
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

    describe "failure" $ do
      it "∅ ⊦ λx. y ⇒ fail" $ do
        let
          ctx  = Context $ M.empty
          term = Abs (ValueVarName 0) (Var $ ValueVarName 1)
          seed = TypeVarNameSeed $ TypeVarName 0
        evalState (runExceptT $ ctype ctx term) seed `shouldSatisfy` isLeft

  describe "CT-App" $ do
    describe "success" $ do
      it "∅ ⊦ λx. true 0 : S | {T, S} {T → Bool = Nat → S}" $ do
        let
          ctx  = Context $ M.empty
          term  = App (Abs (ValueVarName 0) TTrue) Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = TypeVar $ TypeVarName 1
          cons  = S.singleton $ Constraint (Arrow (TypeVar $ TypeVarName 0) TBool) (Arrow Nat (TypeVar $ TypeVarName 1))
          seed' = TypeVarNameSeed $ TypeVarName 2
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

  describe "CT-Zero" $ do
    describe "success" $ do
      it "∅ ⊦ 0 : Nat | ∅ {}" $ do
        let
          ctx   = Context $ M.empty
          term  = Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = Nat
          cons  = S.empty
          seed' = TypeVarNameSeed $ TypeVarName 0
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')


  describe "CT-Succ" $ do
    describe "success" $ do
      it "∅ ⊦ succ 0 : Nat | ∅ {}" $ do
        let
          ctx   = Context $ M.empty
          term  = Succ Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = Nat
          cons  = S.singleton $ Constraint Nat Nat
          seed' = TypeVarNameSeed $ TypeVarName 0
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

  describe "CT-Pred" $ do
    describe "success" $ do
      it "∅ ⊦ pred 0 : Nat | ∅ {}" $ do
        let
          ctx   = Context $ M.empty
          term  = Pred Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = Nat
          cons  = S.singleton $ Constraint Nat Nat
          seed' = TypeVarNameSeed $ TypeVarName 0
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

  describe "CT-IsZero" $ do
    describe "success" $ do
      it "∅ ⊦ iszero 0 : Nat | ∅ {}" $ do
        let
          ctx   = Context $ M.empty
          term  = IsZero Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = TBool
          cons  = S.singleton $ Constraint Nat Nat
          seed' = TypeVarNameSeed $ TypeVarName 0
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

  describe "CT-True" $ do
    describe "success" $ do
      it "∅ ⊦ true : Bool | ∅ {}" $ do
        let
          ctx   = Context $ M.empty
          term  = TTrue
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = TBool
          cons  = S.empty
          seed' = TypeVarNameSeed $ TypeVarName 0
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

  describe "CT-False" $ do
    describe "success" $ do
      it "∅ ⊦ false : Bool | ∅ {}" $ do
        let
          ctx   = Context $ M.empty
          term  = TFalse
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = TBool
          cons  = S.empty
          seed' = TypeVarNameSeed $ TypeVarName 0
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

  describe "CT-If" $ do
    describe "success" $ do
      it "∅ ⊦ if true then 0 else 0 : Bool | ∅ {}" $ do
        let
          ctx   = Context $ M.empty
          term  = If TTrue Zero Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = Nat
          cons  = S.fromList [Constraint TBool TBool, Constraint Nat Nat]
          seed' = TypeVarNameSeed $ TypeVarName 0
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

  describe "CT-LetPoly'" $ do
    describe "success" $ do
      it "∅ ⊦ let id = λ x. x in if id true then id 0 else 0 : X4 | {X1, X2, X3, X4} {X1 → X1 = Bool → X2, X2 = Bool, X3 → X3 = Nat → X4, X4 = Nat}" $ do
        let
          ctx   = Context $ M.empty
          term  = Let (ValueVarName 0) (Abs (ValueVarName 1) (Var $ ValueVarName 1)) (If (App (Var $ ValueVarName 0) TTrue) (App (Var $ ValueVarName 0) Zero) Zero)
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = TypeVar $ TypeVarName 4
          cons  = S.fromList [ Constraint (Arrow (TypeVar $ TypeVarName 1) (TypeVar $ TypeVarName 1)) (Arrow TBool (TypeVar $ TypeVarName 2))
                             , Constraint (TypeVar $ TypeVarName 2) TBool
                             , Constraint (Arrow (TypeVar $ TypeVarName 3) (TypeVar $ TypeVarName 3)) (Arrow Nat (TypeVar $ TypeVarName 4))
                             , Constraint (TypeVar $ TypeVarName 4) Nat
                             ]
          seed' = TypeVarNameSeed $ TypeVarName 5
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')

  describe "principal solution" $ do
    describe "success" $ do
      it "x : Nat ⊦ x : Nat" $ do
        let
          ctx   = Context $ M.singleton (ValueVarName 0) Nat
          term  = Var $ ValueVarName 0
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = Nat
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "x : Bool ⊦ x : Bool" $ do
        let
          ctx   = Context $ M.singleton (ValueVarName 0) TBool
          term = Var $ ValueVarName 0
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = TBool
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "x : X ⊦ x : X" $ do
        let
          ctx = Context $ M.singleton (ValueVarName 0) (TypeVar $ TypeVarName 0)
          term = Var $ ValueVarName 0
          seed  = TypeVarNameSeed $ TypeVarName 1
          asgns = []
          typ   = TypeVar $ TypeVarName 0
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "x : ∀X. X ⊦ x : S" $ do
        let
          ctx   = Context $ M.singleton (ValueVarName 0) (Scheme (S.singleton $ TypeVarName 0) (TypeVar $ TypeVarName 0))
          term  = Var $ ValueVarName 0
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = TypeVar $ TypeVarName 0
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "x : ∀X Y. X ⊦ x : S1" $ do
        let
          ctx   = Context $ M.singleton (ValueVarName 0) (Scheme (S.fromList $ map TypeVarName [0, 1]) (TypeVar $ TypeVarName 0))
          term  = Var $ ValueVarName 0
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = TypeVar $ TypeVarName 0
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "x : ∀X Y. X → Y ⊦ x : S1 → S2" $ do
        let
          ctx   = Context $ M.singleton (ValueVarName 0) (Scheme (S.fromList $ map TypeVarName [0, 1]) (Arrow (TypeVar $ TypeVarName 0) (TypeVar $ TypeVarName 1)))
          term  = Var $ ValueVarName 0
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = Arrow (TypeVar $ TypeVarName 0) (TypeVar $ TypeVarName 1)
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "∅ ⊦ λx. x : S → S" $ do
        let
          ctx   = Context $ M.empty
          term  = Abs (ValueVarName 0) (Var $ ValueVarName 0)
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = Arrow (TypeVar $ TypeVarName 0) (TypeVar $ TypeVarName 0)
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "∅ ⊦ λx. true 0 : Bool" $ do
        let
          ctx  = Context $ M.empty
          term  = App (Abs (ValueVarName 0) TTrue) Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = [ Assign $ M.singleton (TypeVarName 0) Nat
                  , Assign $ M.singleton (TypeVarName 1) TBool
                  ]
          typ   = TBool
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "∅ ⊦ 0 : Nat" $ do
        let
          ctx   = Context $ M.empty
          term  = Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = Nat
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "∅ ⊦ succ 0 : Nat" $ do
        let
          ctx   = Context $ M.empty
          term  = Succ Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = Nat
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "∅ ⊦ pred 0 : Nat" $ do
        let
          ctx   = Context $ M.empty
          term  = Pred Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = Nat
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "∅ ⊦ iszero 0 : Nat" $ do
        let
          ctx   = Context $ M.empty
          term  = IsZero Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = TBool
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "∅ ⊦ true : Bool" $ do
        let
          ctx   = Context $ M.empty
          term  = TTrue
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = TBool
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "∅ ⊦ false : Bool" $ do
        let
          ctx   = Context $ M.empty
          term  = TFalse
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = TBool
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "∅ ⊦ if true then 0 else 0 : Bool" $ do
        let
          ctx   = Context $ M.empty
          term  = If TTrue Zero Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = []
          typ   = Nat
        prinso ctx term seed `shouldBe` Right (asgns, typ)

      it "∅ ⊦ let id = λ x. x in if id true then id 0 else 0 : Nat" $ do
        let
          ctx   = Context $ M.empty
          term  = Let (ValueVarName 0) (Abs (ValueVarName 1) (Var $ ValueVarName 1)) (If (App (Var $ ValueVarName 0) TTrue) (App (Var $ ValueVarName 0) Zero) Zero)
          seed  = TypeVarNameSeed $ TypeVarName 0
          asgns = [ Assign $ M.singleton (TypeVarName 2) TBool
                  , Assign $ M.singleton (TypeVarName 4) Nat
                  , Assign $ M.singleton (TypeVarName 1) TBool
                  , Assign $ M.singleton (TypeVarName 3) Nat
                  ]
          typ   = Nat
        prinso ctx term seed `shouldBe` Right (asgns, typ)

    describe "failure" $ do
      it "∅ ⊦ x ⇒ fail" $ do
        let
          ctx  = Context $ M.empty
          term = Var $ ValueVarName 0
          seed = TypeVarNameSeed $ TypeVarName 0
        prinso ctx term seed `shouldSatisfy` isLeft

      it "∅ ⊦ λx. y ⇒ fail" $ do
        let
          ctx  = Context $ M.empty
          term = Abs (ValueVarName 0) (Var $ ValueVarName 1)
          seed = TypeVarNameSeed $ TypeVarName 0
        prinso ctx term seed `shouldSatisfy` isLeft

  describe "parser" $ do
    describe "var" $ do
      describe "success" $ do
        it "0" $ do
          let
            script = "0"
            term   = Var (ValueVarName 0)
          parse pterm "test var" script `shouldBe` Right term

    describe "zero" $ do
      describe "success" $ do
        it "zero" $ do
          let
            script = "zero"
            term   = Zero
          parse pterm "test zero" script `shouldBe` Right term

    describe "true" $ do
      describe "success" $ do
        it "true" $ do
          let
            script = "true"
            term   = TTrue
          parse pterm "test true" script `shouldBe` Right term

    describe "false" $ do
      describe "success" $ do
        it "false" $ do
          let
            script = "false"
            term   = TFalse
          parse pterm "test false" script `shouldBe` Right term

    describe "succ" $ do
      describe "success" $ do
        it "succ zero" $ do
          let
            script = "succ zero"
            term   = Succ Zero
          parse pterm "test succ" script `shouldBe` Right term

    describe "pred" $ do
      describe "success" $ do
        it "pred zero" $ do
          let
            script = "pred zero"
            term   = Pred Zero
          parse pterm "test pred" script `shouldBe` Right term

    describe "iszero" $ do
      describe "success" $ do
        it "iszero zero" $ do
          let
            script = "iszero zero"
            term   = IsZero Zero
          parse pterm "test iszero" script `shouldBe` Right term

    describe "if" $ do
      describe "success" $ do
        it "if true then true else true" $ do
          let
            script = "if true then true else true"
            term   = If TTrue TTrue TTrue
          parse pterm "test if" script `shouldBe` Right term

    describe "abs" $ do
      describe "success" $ do
        it "λ0. true" $ do
          let
            script = "λ0. true"
            term   = Abs (ValueVarName 0) TTrue
          parse pterm "test abs" script `shouldBe` Right term

    describe "let" $ do
      describe "success" $ do
        it "let 0 = true in true" $ do
          let
            script = "let 0 = true in true"
            term   = Let (ValueVarName 0) TTrue TTrue
          parse pterm "test let" script `shouldBe` Right term

    describe "paren" $ do
      describe "success" $ do
        it "(true)" $ do
          let
            script = "(true)"
            term   = TTrue
          parse pterm "test paren" script `shouldBe` Right term

    describe "app" $ do
      describe "success" $ do
        it "true true" $ do
          let
            script = "true true"
            term   = App TTrue TTrue
          parse pterm "test app" script `shouldBe` Right term
