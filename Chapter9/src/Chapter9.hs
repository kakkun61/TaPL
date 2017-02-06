module Chapter9 where

import Data.Set (Set (..))
import qualified Data.Set as Set

type VarId = String

data Term = Var VarId
          | Abs VarId Type Term
          | App Term Term

data Type = Arrow Type Type

type Context = Set VarId Type

eval :: Term -> Maybe Term

-- E-App
eval (App t1 t2) = case eval t1 of
                       Just t1' -> Just $ App t1' t2
                       Nothing  -> case eval t2 of
                                       Just t2' -> Just $ App t1 t2'
                                       Nothing  -> evalAppAbs t1 t2
                   where
                       evalAppAbs (App (Abs varId varType t12) v2) =
                           assign varId v2 t12
eval _           = Nothing

assign :: VarId -> Type -> Term -> Term
assign 