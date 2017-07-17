module SystemFOmega.Type ( ValueVarName (..)
                         , TypeVarName (..)
                         , Term (..)
                         , Type (..)
                         , Bind (..)
                         , Context
                         , Kind
                         ) where

import Data.Text

newtype ValueVarName = ValueVarName Text
  deriving (Eq, Ord, Show)

newtype TypeVarName = TypeVarName Text
  deriving (Eq, Ord, Show)

data Term = Var ValueVarName
          | Abs ValueVarName Type Term
          | App Term Term
          deriving (Eq, Show)

data Type = TypeVar TypeVarName
          | TypeAbs TypeVarName Kind Type
          | TypeApp Type Type
          | Arrow Type Type
          deriving (Eq, Show)

data Bind = ValueVarBind ValueVarName Type
          | TypeVarBind TypeVarName Kind
          deriving (Eq, Show)

type Context = [Bind]

data Kind = AtomKind
          | ArrowKind Kind Kind
          deriving (Eq, Show)
