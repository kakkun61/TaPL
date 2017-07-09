module SystemFOmega.Type ( ValueVarName (..)
                         , TypeVarName (..)
                         , Term (..)
                         , Type (..)
                         , Context (..)
                         ) where

import Data.Set (Set)
import Data.Map.Strict (Map)

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
