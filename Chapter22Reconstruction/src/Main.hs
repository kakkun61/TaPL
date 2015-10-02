module Main where

import Data.Set (Set, (\\))
import qualified Data.Set as S

type VarName = String

data Term = Var VarName
          | Zero
          | TTrue
          | TFalse
          | Succ Term
          | Pred Term
          | If Term Term Term
          | Lambda String Term
          | Let VarName Term Term

data Type = TypeVar VarName
          | Nat
          | TBool
          | Arrow Type Type
          | Scheme (Set VarName) Type

type Context = Set (VarName, Type)

type Constraint = (Type, Type)

type Assign = (VarName, Type)

ftv :: Type -> Set VarName
ftv (TypeVar v) = S.singleton v
ftv Nat = S.empty
ftv TBool = S.empty
ftv (Arrow t1 t2) = S.union (ftv t1) (ftv t2)
ftv (Scheme vs t) = ftp t \\ vs

unify :: Set Constraint -> Maybe [Assign]
unify c =
  if S.null c
  then []
  else
    let (s, t)

ct :: Context -> Term -> (Type, Set VarName, Set Constraint)
ct = undefined
