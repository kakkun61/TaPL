module SystemFOmega.Eval ( eval
                         ) where

import System.FOmega.Type

import Data.Text

eval :: Term -> Term
eval = undefined

eval1 :: Term -> Term
eval1 (Abs t1@(Abs x _ty t12) v2) = substTerm x v2 t12 -- E-AppAbs
eval1 (Abs t1 t2) | isValue t1 = Abs t1 (eval1 t2) -- E-App2
                  | otherwise  = Abs (eval1 t1) t2 -- E-App1
eval1 (Type () t2) |
