module Recon.Parse (parse) where

import Recon.Type

import Text.Parsec (Parsec, ParseError, (<|>))
import qualified Text.Parsec as P
import Data.Text (Text)

type Parser = Parsec Text ()

parse :: Text -> Either ParseError Term
parse s = P.parse pterm "recon" s

pterm :: Parser Term
pterm =
  P.try (P.chainl1 pterma $ P.spaces >> return App) <|> pterma

pterma :: Parser Term
pterma =
  P.choice $ map P.try [ pvart
                       , pzero
                       , ptrue
                       , pfalse
                       , psucc
                       , ppred
                       , piszero
                       , pif
                       , pabs
                       , plet
                       , P.between (P.char '(') (P.char ')') pterm
                       ]

pvar :: Parser ValueVarName
pvar = do
  fmap (ValueVarName . read) $
    P.try $ do
      h <- P.oneOf ['1' .. '9']
      tl <- P.many P.digit
      return $ h:tl
    <|> do
      z <- P.char '0'
      return [z]

pvart :: Parser Term
pvart = do
  x <- pvar
  return $ Var x

pzero :: Parser Term
pzero = do
  _ <- P.string "zero"
  return Zero

ptrue :: Parser Term
ptrue = do
  _ <- P.string "true"
  return TTrue

pfalse :: Parser Term
pfalse = do
  _ <- P.string "false"
  return TFalse

psucc :: Parser Term
psucc = do
  _ <- P.string "succ"
  P.spaces
  t <- pterm
  return $ Succ t

ppred :: Parser Term
ppred = do
  _ <- P.string "pred"
  P.spaces
  t <- pterm
  return $ Pred t

piszero :: Parser Term
piszero = do
  _ <- P.string "iszero"
  P.spaces
  t <- pterm
  return $ IsZero t

pif :: Parser Term
pif = do
  _ <- P.string "if"
  P.spaces
  t1 <- pterm
  P.spaces
  _ <- P.string "then"
  P.spaces
  t2 <- pterm
  P.spaces
  _ <- P.string "else"
  P.spaces
  t3 <- pterm
  return $ If t1 t2 t3

pabs :: Parser Term
pabs = do
  _ <- P.oneOf ['λ', '\\']
  P.spaces
  x <- pvar
  P.spaces
  _ <- P.char '.'
  P.spaces
  t <- pterm
  return $ Abs x t

papp :: Parser Term
papp = do
  t1 <- pterm
  P.spaces
  t2 <- pterm
  return $ App t1 t2

plet :: Parser Term
plet = do
  _ <- P.string "let"
  P.spaces
  x <- pvar
  P.spaces
  _ <- P.char '='
  P.spaces
  t1 <- pterm
  P.spaces
  _ <- P.string "in"
  P.spaces
  t2 <- pterm
  return $ Let x t1 t2

pdelimiter :: Parser ()
pdelimiter = do
  P.lookAhead (P.char '(' >> return ()) <|> P.skipMany1 P.space
