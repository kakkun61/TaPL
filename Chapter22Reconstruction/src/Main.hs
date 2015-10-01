module Main where

data Term = Var String
          | Zero
          | TTrue
          | TFalse
          | Succ Term
          | Pred Term
          | If Term Term Term
          | Lambda String Term
          | Let String Term Term

data Type = TypeVar String
          | Nam
          | TBool
          | Arrow Type Type
          | Scheme [String] Type
