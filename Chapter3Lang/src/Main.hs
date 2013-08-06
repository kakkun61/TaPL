data Term = TTrue | TFalse | Zero
          | Succ Term | Pred Term | IsZero Term
          | If Term Term Term
            deriving (Eq, Show)

data EvaledTerm = ETrue | EFalse | EZero
                | ESucc EvaledTerm | EPred EvaledTerm
                  deriving (Eq, Show)

eval :: Term -> EvaledTerm
eval TTrue  = ETrue
eval TFalse = EFalse
eval Zero   = EZero
eval (Succ t) =
    case eval t of
        EPred et -> et
        et       -> ESucc et
eval (Pred t) =
    case eval t of
        ESucc et -> et
        et       -> EPred et
eval (IsZero t) =
    case eval t of
        EZero -> ETrue
        _     -> EFalse
eval (If c t f) =
    case eval c of
        ETrue  -> eval t
        EFalse -> eval f

pretty :: EvaledTerm -> String
pretty ETrue  = "True"
pretty EFalse = "False"
pretty t      = show $ num t

num :: EvaledTerm -> Integer
num (ESucc t) = (num t) + 1
num (EPred t) = (num t) - 1
num EZero     = 0
num _        = error "not evaluated syntax tree"

main = do
    putStrLn $ pretty $ eval $ Succ $ Zero
    putStrLn $ pretty $ eval $
            If (IsZero $ Pred $ Succ $ Zero)
               (Succ $ Succ $ Succ $ Zero)
               (Pred $ Zero)
    putStrLn $ pretty $ eval $
            IsZero (Pred (If TTrue (Succ Zero) Zero))
