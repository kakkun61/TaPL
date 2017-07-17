module SystemFOmega.Eval ( eval
                         ) where

import System.FOmega.Type

import Data.Text

eval :: Term -> Term
eval = undefined

eval1 :: Term -> Term
eval1 (Abs t1@(Abs x ty11 t12) t2) = 
eval1 (Abs t1 t2) | isValue t1 = Abs t1 (eval1 t2) -- E-App2
                  | otherwise  = Abs (eval1 t1) t2 -- E-App1
eval1 (Type () t2) |
