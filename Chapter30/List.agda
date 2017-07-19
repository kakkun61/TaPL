module List where

open import Data.Nat
open import Function

data List : ℕ → Set → Set where
  nil : ∀ {a} → List zero a
  cons : ∀ {n a} → a → List n a → List (suc n) a

hd : ∀ {n a} → List (suc n) a → a
hd (cons x l) = x

tl : ∀ {n a} → List (suc n) a → List n a
tl (cons x l) = l

1-to-3 = cons 1 $ cons 2 $ cons 3 nil
