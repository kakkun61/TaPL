module Recon ( ValueVarName (..)
             , TypeVarName (..)
             , Term (..)
             , Type (..)
             , Context (..)
             , Constraint (..)
             , Assign (..)
             , TypeVarNameSeed (..)
             , assign
             , assigns
             , ctype
             , prinso
             , prinso'
             ) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.Foldable
import Control.Monad.Trans.Except

newtype ValueVarName = ValueVarName Int
  deriving (Eq, Ord, Show)
newtype TypeVarName = TypeVarName Int
  deriving (Eq, Ord, Show)

data Term = Var ValueVarName
          | Zero
          | TTrue
          | TFalse
          | Succ Term
          | Pred Term
          | IsZero Term
          | If Term Term Term
          | Abs ValueVarName Term
          | App Term Term
          | Let ValueVarName Term Term
          deriving (Eq, Show)

data Type = TypeVar TypeVarName
          | Nat
          | TBool
          | Arrow Type Type
          | Scheme (Set TypeVarName) Type
          deriving (Eq, Ord, Show)

newtype Context = Context (Map ValueVarName Type)
  deriving (Show)

data Constraint = Constraint Type Type
  deriving (Eq, Ord, Show)

newtype Assign = Assign (Map TypeVarName Type)
  deriving (Eq, Show)

class Assignable a where
  assign :: Assign -> a -> a
  assigns :: [Assign] -> a -> a
  assigns as t = foldl (flip assign) t as

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

fv :: Type -> Set TypeVarName
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

newtype TypeVarNameSeed = TypeVarNameSeed TypeVarName
  deriving (Eq, Show)

-- | gensym
genTypeVarName :: State TypeVarNameSeed TypeVarName
genTypeVarName = state genTypeVarName'
  where
    genTypeVarName' :: TypeVarNameSeed -> (TypeVarName, TypeVarNameSeed)
    genTypeVarName' (TypeVarNameSeed s@(TypeVarName n)) = (s, TypeVarNameSeed (TypeVarName(succ n)))

ctype :: Context -> Term -> ExceptT String (State TypeVarNameSeed) (Type, Set Constraint)
ctype (Context ctx) term@(Var x) = do -- CT-Var
  typ <- case M.lookup x ctx of
         Just (Scheme xs s) -> do
                                 a <- Assign <$> foldlM go M.empty xs
                                 return $ assign a s
                                 where
                                   go m x' = lift genTypeVarName >>= \y -> return $ M.insert x' (TypeVar y) m
         Just typ -> return typ
         Nothing -> throwE $ "context has no corresponding type\n\tcontext: " ++ (show $ M.toList ctx) ++ "\n\tterm: " ++ (show term)
  return (typ, S.empty)
ctype (Context ctx) (Abs x t) = do -- CT-AbsInf
  xt <- lift $ TypeVar <$> genTypeVarName
  let ctx' = Context $ M.insert x xt ctx
  (typ, cons) <- ctype ctx' t
  return (Arrow xt typ, cons)
ctype ctx (App t1 t2) = do -- CT-App
  (typ1, cons1) <- ctype ctx t1
  (typ2, cons2) <- ctype ctx t2
  typ <- lift $ TypeVar <$> genTypeVarName
  let cons = S.unions [cons1, cons2, S.singleton $ Constraint typ1 (Arrow typ2 typ)]
  return (typ, cons)
ctype _ Zero = do -- CT-Zero
  return (Nat, S.empty)
ctype ctx (Succ t) = do -- CT-Succ
  (typ, cons) <- ctype ctx t
  let cons' = S.insert (Constraint typ Nat) cons
  return (Nat, cons')
ctype ctx (Pred t) = do -- CT-Pred
  (typ, cons) <- ctype ctx t
  let cons' = S.insert (Constraint typ Nat) cons
  return (Nat, cons')
ctype ctx (IsZero t) = do -- CT-IsZero
  (typ, cons) <- ctype ctx t
  let cons' = S.insert (Constraint typ Nat) cons
  return (TBool, cons')
ctype _ TTrue = do -- CT-True
  return (TBool, S.empty)
ctype _ TFalse = do -- CT-False
  return (TBool, S.empty)
ctype ctx (If t1 t2 t3) = do -- CT-If
  (typ1, cons1) <- ctype ctx t1
  (typ2, cons2) <- ctype ctx t2
  (typ3, cons3) <- ctype ctx t3
  let cons = S.unions [cons1, cons2, cons3, S.fromList [Constraint typ1 TBool, Constraint typ2 typ3]]
  return (typ2, cons)
ctype ctx@(Context mctx) (Let x t1 t2) = do -- CT-LetPoly'
  (typ1, cons1) <- ctype ctx t1
  case unify cons1 of
    Right asgns -> do
                 let
                   typ1' = assigns asgns typ1
                   tvs = [tv | tv <- S.toList $ fv typ1'
                             , tv `notElem` concatMap (S.toList . fv) (M.elems mctx)
                         ]
                   ctx' = Context $ M.insert x (Scheme (S.fromList tvs) typ1') mctx
                 ctype ctx' t2
    Left s -> throwE $ s ++ " at CT-LetPoly'"

-- | 主要解 principal solution
prinso :: Context -> Term -> TypeVarNameSeed -> Either String ([Assign], Type)
prinso ctx term seed =
  case evalState (runExceptT $ ctype ctx term) seed of
    Right (typ, cons) -> prinso' typ cons
    Left s -> Left s

prinso' :: Type -> Set Constraint -> Either String ([Assign], Type)
prinso' typ cons =
  case unify cons of
    Right asgns -> Right (asgns, assigns asgns typ)
    Left s -> Left s
