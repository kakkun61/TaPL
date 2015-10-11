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
          c = Context $ M.singleton 0 Nat
          t = Var 0
        runState (runExceptT $ ctype c t) (varNameSeed 0) `shouldBe` (Right (Nat, S.fromList []), VarNameSeed 0)

      it "x : Bool ⊦ x : Bool | {} {}" $ do
        let
          c = Context $ M.singleton 0 TBool
          t = Var 0
        runState (runExceptT $ ctype c t) (varNameSeed 0) `shouldBe` (Right (TBool, S.fromList []), VarNameSeed 0)

      it "x : X ⊦ x : X | {} {}" $ do
        let
          c = Context $ M.singleton 0 (TypeVar 0)
          t = Var 0
        runState (runExceptT $ ctype c t) (varNameSeed 1) `shouldBe` (Right (TypeVar 0, S.fromList []), VarNameSeed 1)

      it "x : ∀X. X ⊦ x : ∀S.S | {S} {}" $ do
        let
          c = Context $ M.singleton 0 (Scheme (S.singleton 0) (TypeVar 0))
          t = Var 0
        runState (runExceptT $ ctype c t) (varNameSeed 0) `shouldBe` (Right (TypeVar 0, S.fromList []), VarNameSeed 1)

      it "x : ∀X Y. X ⊦ x : S1 | {S1, S2} {}" $ do
        let
          c = Context $ M.singleton 0 (Scheme (S.fromList [0, 1]) (TypeVar 0))
          t = Var 0
        runState (runExceptT $ ctype c t) (varNameSeed 0) `shouldBe` (Right (TypeVar 0, S.fromList []), VarNameSeed 2)

      it "x : ∀X Y. X → Y ⊦ x : S1 → S2 | {S1, S2} {}" $ do
        let
          c = Context $ M.singleton 0 (Scheme (S.fromList [0, 1]) (Arrow (TypeVar 0) (TypeVar 1)))
          t = Var 0
        runState (runExceptT $ ctype c t) (varNameSeed 0) `shouldBe` (Right (Arrow (TypeVar 0) (TypeVar 1), S.fromList []), VarNameSeed 2)

    describe "failure" $ do
      it "∅ ⊦ x ⇒ fail" $ do
        let
          c = Context $ M.empty
          t = Var 0
        evalState (runExceptT $ ctype c t) (varNameSeed 0) `shouldSatisfy` isLeft
