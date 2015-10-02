module Main where

import Data.Set (Set, (\\))
import qualified Data.Set as S

type VarName = Int

data Term = Var VarName
          | Zero
          | TTrue
          | TFalse
          | Succ Term
          | Pred Term
          | If Term Term Term
          | Lambda String Term
          | Let VarName Term Term
          deriving (Eq, Show)

data Type = TypeVar VarName
          | Nat
          | TBool
          | Arrow Type Type
          | Scheme (Set VarName) Type
          deriving (Eq, Show)

type Context = Set (VarName, Type)

data Constraint = Constraint Type Type
  deriving (Ord, Show)

-- 同時代入が存在するからそれに適応しないと
data Assign = Assign VarName Type
  deriving (Show)

class Assignable a where
  assign :: Assign -> a -> a

instance Assignable Type where
  assign a (TypeVar n) = undefined
  assign _ Nat = Nat
  assign _ TBool = TBool
  assign a (Arrow t1 t2) = Arrow (assign a t1) (assign a t2)
  assign a (Scheme v t) = Scheme v (assign a t)

instance Assignable Constraint where
  assign (Constraint t1 t2) = Constraint (assign a t1) (assign a t2)

fv :: Type -> Set VarName
fv (TypeVar v) = S.singleton v
fv Nat = S.empty
fv TBool = S.empty
fv (Arrow t1 t2) = S.union (ftv t1) (ftv t2)
fv (Scheme vs t) = ftp t \\ vs

-- | 単一化関数
-- | 代入のリストは先に適用するものが先頭である。
unify :: Set Constraint -> Either String [Assign]
unify c =
  if S.null c
  then Right []
  else
    let (Constraint s t) = S.elemAt 0 c
        c' = S.deleteAt 0 c
    in
      case (s, t) of
        (_, _) | s == t => unify c'
        (TypeVar n, _) | not (S.member n (fv t)) => let a = Assign n t
                                                    in unify (assign a c) >>= (a:)
        (_, TypeVar n) | not (S.member n (fv s)) => let a = Assign n s
                                                    in unify (assign a c) >>= (a:)
        ((Arrow s1 s2), (Arrow t1 r2)) => unify (S.insert (Constraint s1 t1)
                                                (S.insert (Constraint s2 t2) c'))
        _ => Left "invalid constraints"

newtype VarNameSeed = VarNameSeed VarName

varNameSeed :: VarName -> VarNameSeed
varNameSeed = VarNameSeed

genVarName :: VarNameSeed -> (VarName, VarNameSeed)
genVarName (VarNameSeed s) = (s, varNameSeed (succ s))

ctype :: Context -> Term -> VarNameSeed -> (Type, Set VarName, Set Constraint, VarNameSeed)
ctype = undefined
