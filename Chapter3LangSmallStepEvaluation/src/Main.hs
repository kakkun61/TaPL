data Term = TTrue | TFalse | Zero
          | Succ Term | Pred Term | IsZero Term
          | If Term Term Term
            deriving (Show)

value :: Term -> Bool
value TTrue  = True
value TFalse = True
value t      = number t
    where
        number :: Term -> Bool
        number Zero     = True
        number (Succ t) = number t
        number _        = False

evaluate :: Term -> Term
evaluate t =
    if value t
    then t
    else eval t

eval :: Term -> Term
-- E-IfTrue
eval (If TTrue t f) = t
-- E-IfFalse
eval (If TFalse t f) = f
-- E-If
eval (If c t f) = If (eval c) t f
-- E-Succ
eval (Succ t) = Succ (eval t)
-- E-PredZero
eval (Pred Zero) = Zero
-- E-PredSucc
eval (Pred (Succ t)) = t
-- E-Pred
eval (Pred t) = Pred (eval t)
-- E-IsZeroZero
eval (IsZero Zero) = TTrue
-- E-IsZeroSucc
eval (IsZero (Succ t)) = TFalse
-- E-IsZero
eval (IsZero t) = IsZero (eval t)

pretty :: Term -> String
pretty TTrue  = "True"
pretty TFalse = "False"
pretty t      = show $ num t

num :: Term -> Integer
num (Succ t) = (num t) + 1
num Zero     = 0

main = do
    putStrLn $ pretty $ evaluate $ Succ $ Zero
    putStrLn $ pretty $ evaluate $
            If (IsZero $ Pred $ Succ $ Zero)
               (Succ $ Succ $ Succ $ Zero)
               (Pred $ Zero)
    putStrLn $ pretty $ evaluate $
            IsZero (Pred (If TTrue (Succ Zero) Zero))
