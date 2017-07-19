module SystemFOmega.Type ( ValueVarName (..)
                         , TypeVarName (..)
                         , Term (..)
                         , Type (..)
                         , Bind (..)
                         , Context
                         , Kind
                         ) where

newtype ValueVarName = ValueVarName Int
  deriving (Eq, Ord, Show)

newtype TypeVarName = TypeVarName Int
  deriving (Eq, Ord, Show)

data Term = Var ValueVarName              -- x
          | Abs ValueVarName Type Term    -- λx:T. t
          | App Term Term                 -- t t
          | TypeAbs TypeVarName Kind Term -- λT:K. t
          | TypeApp Term Type             -- t[T]
          deriving (Eq, Show)

data Type = TypeVar TypeVarName              -- X
          | All TypeVarName Kind Type        -- ∀X:K. T
          | TypeOppAbs TypeVarName Kind Type -- λX:K. T
          | TypeOppApp Type Type             -- T T
          | Arrow Type Type                  -- T→T
          deriving (Eq, Show)

data Bind = ValueVarBind ValueVarName Type
          | TypeVarBind TypeVarName Kind
          deriving (Eq, Show)

type Context = [Bind]

data Kind = AtomKind
          | OppKind Kind Kind
          deriving (Eq, Show)
