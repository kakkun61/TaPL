module Chapter9 where

type DBIndex = Int

type VarName = String

data Term = Var DBIndex
          | Abs VarName Type Term
          | App Term Term
          | TTrue
          | TFalse
            deriving (Show)

data Type = Arrow Type Type
          | Bool
            deriving (Show, Eq)


type Context = [Binding]

data Binding = VarBind VarName Type

-- 図 9-1
eval :: Term -> Maybe Term
eval (App t1 t2) = case eval t1 of
                       Just t1' -> Just $ App t1' t2
                       Nothing  -> case eval t2 of
                                       Just t2' -> Just $ App t1 t2'
                                       Nothing  -> evalAppAbs t1 t2
                   where
                       evalAppAbs (Abs _ _ t12) v2 = Just $ substTop v2 t12
                       evalAppAbs _ _ = error $ "E-AppAbs: t1: expected: abstraction, actual: " ++ (show t1)
eval _           = Nothing

-- 7.2
-- 定義 6.2.1
shift :: DBIndex -> Term -> Term
shift d t = go 0 t
            where
                go c (Var x) | c <= x = Var (x + d)
                             | True   = Var x
                go c (Abs n ty t2)    = Abs n ty $ go (c + 1) t2
                go c (App t1 t2)      = App (go c t1) (go c t2)
                go _ TTrue            = TTrue
                go _ TFalse           = TFalse

-- 定義 6.2.4
subst :: DBIndex -> Term -> Term -> Term
subst j s t = go 0 t
              where
                  go c (Var x) | x == j + c = shift c s
                               | True       = Var x
                  go c (Abs n ty t1)        = Abs n ty $ go (c + 1) t1
                  go c (App t1 t2)          = App (go c t1) (go c t2)
                  go _ TTrue            = TTrue
                  go _ TFalse           = TFalse

substTop :: Term -> Term -> Term
substTop s t = shift (-1) $ subst 0 (shift 1 s) t

-- 図 9-1
typeof :: Context -> Term -> Either String Type
typeof ctx (Var x) = case ctx !! x of
                         VarBind _ ty -> Right ty
typeof ctx (Abs x ty1 t2) = let ctx'      = (VarBind x ty1):ctx
                                Right ty2 = typeof ctx' t2
                            in Right $ Arrow ty1 ty2
typeof ctx (App t1 t2)    = let Right ty1 = typeof ctx t1
                                Right ty2 = typeof ctx t2
                            in case ty1 of
                                   Arrow ty11 ty12 | ty2 == ty11 -> Right ty12
                                                   | True        -> Left $ "T-App: mismatch: parameter: " ++ (show ty11) ++ ", argument: " ++ (show ty2)
                                   _ -> Left $ "T-App: expected: arrow, actual: " ++ (show ty1)
typeof _ TTrue            = Right Bool
typeof _ TFalse           = Right Bool
