module ReconSpec where

import Test.Hspec
import Recon
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Either

spec :: Spec
spec = do
  describe "CT-Var" $ do
    describe "success" $ do
      it "x : Nat ⊦ x : Nat | {} {}" $ do
        let
          c = Context $ M.singleton (ValueVarName 0) Nat
          t = Var $ ValueVarName 0
        runState (runExceptT $ ctype c t) (TypeVarNameSeed $ TypeVarName 0) `shouldBe` (Right (Nat, S.empty), TypeVarNameSeed $ TypeVarName 0)

      it "x : Bool ⊦ x : Bool | {} {}" $ do
        let
          c = Context $ M.singleton (ValueVarName 0) TBool
          t = Var $ ValueVarName 0
        runState (runExceptT $ ctype c t) (TypeVarNameSeed $ TypeVarName 0) `shouldBe` (Right (TBool, S.empty), TypeVarNameSeed $ TypeVarName 0)

      it "x : X ⊦ x : X | {} {}" $ do
        let
          c = Context $ M.singleton (ValueVarName 0) (TypeVar $ TypeVarName 0)
          t = Var $ ValueVarName 0
        runState (runExceptT $ ctype c t) (TypeVarNameSeed $ TypeVarName 1) `shouldBe` (Right (TypeVar $ TypeVarName 0, S.empty), TypeVarNameSeed $ TypeVarName 1)

      it "x : ∀X. X ⊦ x : ∀S.S | {S} {}" $ do
        let
          c = Context $ M.singleton (ValueVarName 0) (Scheme (S.singleton $ TypeVarName 0) (TypeVar $ TypeVarName 0))
          t = Var $ ValueVarName 0
        runState (runExceptT $ ctype c t) (TypeVarNameSeed $ TypeVarName 0) `shouldBe` (Right (TypeVar $ TypeVarName 0, S.empty), TypeVarNameSeed $ TypeVarName 1)

      it "x : ∀X Y. X ⊦ x : S1 | {S1, S2} {}" $ do
        let
          c = Context $ M.singleton (ValueVarName 0) (Scheme (S.fromList $ map TypeVarName [0, 1]) (TypeVar $ TypeVarName 0))
          t = Var $ ValueVarName 0
        runState (runExceptT $ ctype c t) (TypeVarNameSeed $ TypeVarName 0) `shouldBe` (Right (TypeVar $ TypeVarName 0, S.empty), TypeVarNameSeed $ TypeVarName 2)

      it "x : ∀X Y. X → Y ⊦ x : S1 → S2 | {S1, S2} {}" $ do
        let
          c = Context $ M.singleton (ValueVarName 0) (Scheme (S.fromList $ map TypeVarName [0, 1]) (Arrow (TypeVar $ TypeVarName 0) (TypeVar $ TypeVarName 1)))
          t = Var $ ValueVarName 0
        runState (runExceptT $ ctype c t) (TypeVarNameSeed $ TypeVarName 0) `shouldBe` (Right (Arrow (TypeVar $ TypeVarName 0) (TypeVar $ TypeVarName 1), S.empty), TypeVarNameSeed $ TypeVarName 2)

    describe "failure" $ do
      it "∅ ⊦ x ⇒ fail" $ do
        let
          c = Context $ M.empty
          t = Var $ ValueVarName 0
        evalState (runExceptT $ ctype c t) (TypeVarNameSeed $ TypeVarName 0) `shouldSatisfy` isLeft

  describe "CT-Abs" $ do
    describe "success" $ do
      it "∅ ⊦ λx. x : S → S | {S} {}" $ do
        pending
        let
          c = Context $ M.empty
          t = Lambda (ValueVarName 0) (Var $ ValueVarName 0)
        runState (runExceptT $ ctype c t) (TypeVarNameSeed $ TypeVarName 0) `shouldBe` (Right (Arrow (TypeVar $ TypeVarName 0) (TypeVar $ TypeVarName 1), S.empty), TypeVarNameSeed $ TypeVarName 2)

    describe "failure" $ do
      it "∅ ⊦ λx. y ⇒ fail" $ do
        pending
        let
          c = Context $ M.empty
          t = Lambda (ValueVarName 0) (Var $ ValueVarName 1)
        evalState (runExceptT $ ctype c t) (TypeVarNameSeed $ TypeVarName 0) `shouldSatisfy` isLeft

  describe "CT-App" $ do
    describe "success" $ do
      it "∅ ⊦ λx. true 0 : S | {T, S} {T → Bool = Nat → S}" $ do
         pending
         let
           c = Context $ M.empty
           t = App (Lambda (ValueVarName 0) TTrue) Zero
         runState (runExceptT $ ctype c t) (TypeVarNameSeed $ TypeVarName 0) `shouldBe` (Right ((TypeVar $ TypeVarName 1), S.singleton $ Constraint (Arrow (TypeVar $ TypeVarName 0) TBool) (Arrow Nat (TypeVar $ TypeVarName 1))), TypeVarNameSeed $ TypeVarName 2)
