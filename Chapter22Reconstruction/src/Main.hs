module Main where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type VarName = Int

data Term = Var VarName
          | Zero
          | TTrue
          | TFalse
          | Succ Term
          | Pred Term
          | If Term Term Term
          | Lambda VarName Term
          | Let VarName Term Term
          deriving (Eq, Show)

data Type = TypeVar VarName
          | Nat
          | TBool
          | Arrow Type Type
          | Scheme (Set VarName) Type
          deriving (Eq, Ord, Show)

newtype Context = Context (Map VarName Type)
  deriving (Show)

data Constraint = Constraint Type Type
  deriving (Eq, Ord, Show)

newtype Assign = Assign (Map VarName Type)
  deriving (Show)

class Assignable a where
  assign :: Assign -> a -> a

instance Assignable Type where
  assign (Assign m) t@(TypeVar x) = case M.lookup x m of
                                      Just t' -> t'
                                      Nothing -> t
  assign _ Nat = Nat
  assign _ TBool = TBool
  assign a (Arrow t1 t2) = Arrow (assign a t1) (assign a t2)
  assign a (Scheme v t) = Scheme v (assign a t)

instance Assignable Constraint where
  assign a (Constraint t1 t2) = Constraint (assign a t1) (assign a t2)

instance (Assignable a, Ord a) => Assignable (Set a) where
  assign a s = S.map (assign a) s

fv :: Type -> Set VarName
fv (TypeVar x) = S.singleton x
fv Nat = S.empty
fv TBool = S.empty
fv (Arrow t1 t2) = S.union (fv t1) (fv t2)
fv (Scheme vs t) = fv t S.\\ vs

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
        (_, _) | s == t -> unify c'
        (TypeVar x, _) | not (S.member x (fv t)) -> let a = Assign $ M.singleton x t
                                                    in unify (assign a c) >>= Right . (a:)
        (_, TypeVar x) | not (S.member x (fv s)) -> let a = Assign $ M.singleton x s
                                                    in unify (assign a c) >>= Right . (a:)
        ((Arrow s1 s2), (Arrow t1 t2)) -> unify (S.insert (Constraint s1 t1)
                                                (S.insert (Constraint s2 t2) c'))
        _ -> Left "invalid constraints"

newtype VarNameSeed = VarNameSeed VarName

varNameSeed :: VarName -> VarNameSeed
varNameSeed = VarNameSeed

genVarName :: VarNameSeed -> (VarName, VarNameSeed)
genVarName (VarNameSeed s) = (s, varNameSeed (succ s))

ctype :: Context -> Term -> VarNameSeed -> (Type, Set VarName, Set Constraint, VarNameSeed)
ctype (Context ctx) (Var x) vseed =
  let
    t = case M.lookup x ctx of
          Just (Scheme xs s) -> let
                                  go x' (m, vs) = let (y, vs') = genVerName vseed
                                                  in (M.insert x' y m, vs')
                                  (assign, vs) = S.foldr go (M.empty, vseed) xs
                                in
                                  undefined
          Just (t) -> t
          Nothing ->

main :: IO ()
main = undefined
