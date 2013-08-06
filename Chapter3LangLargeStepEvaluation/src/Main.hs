data Term = TTrue | TFalse | Zero
          | Succ Term | Pred Term | IsZero Term
          | If Term Term Term
            deriving (Show)

data Value = VTrue | VFalse | NV NumberValue
             deriving (Show)

data NumberValue = VZero | VSucc NumberValue
                   deriving (Show)

eval :: Term -> Value
-- B-Value
eval TTrue  = VTrue
eval TFalse = VFalse
eval Zero   = NV VZero
-- B-IfTrue, B-IfFalse
eval (If c t f) =
    case eval c of
        VTrue  -> eval t
        VFalse -> eval f
-- B-Succ
eval (Succ t) =
    case eval t of
        NV nv -> NV $ VSucc nv
-- B-PredZero, B-PredSucc
eval (Pred t) =
    case eval t of
        NV VZero      -> NV VZero
        NV (VSucc nv) -> NV nv
-- B-IsZeroZero, B-IsZeroSucc
eval (IsZero t) =
    case eval t of
        NV VZero    -> VTrue
        NV (VSucc _) -> VFalse

pretty :: Value -> String
pretty VTrue   = "True"
pretty VFalse  = "False"
pretty (NV nv) = show $ num nv

num :: NumberValue -> Integer
num (VSucc t) = (num t) + 1
num VZero     = 0

main = do
    putStrLn $ pretty $ eval $ Succ $ Zero
    putStrLn $ pretty $ eval $
            If (IsZero $ Pred $ Succ $ Zero)
               (Succ $ Succ $ Succ $ Zero)
               (Pred $ Zero)
    putStrLn $ pretty $ eval $
            IsZero (Pred (If TTrue (Succ Zero) Zero))
