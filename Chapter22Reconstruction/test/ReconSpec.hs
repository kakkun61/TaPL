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

      it "x : ∀X. X ⊦ x : ∀S.S | {S} {}" $ do
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
        pending
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
        pending
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
        pending
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
        pending
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
        pending
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
        pending
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
        pending
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
        pending
        let
          ctx   = Context $ M.empty
          term  = If TTrue Zero Zero
          seed  = TypeVarNameSeed $ TypeVarName 0
          typ   = Nat
          cons  = S.fromList [Constraint TBool TBool, Constraint Nat Nat]
          seed' = TypeVarNameSeed $ TypeVarName 0
        runState (runExceptT $ ctype ctx term) seed `shouldBe` (Right (typ, cons), seed')
