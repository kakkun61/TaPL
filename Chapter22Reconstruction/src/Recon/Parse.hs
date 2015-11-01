module Recon.Parse {-(parse)-} where

import Recon.Type

import Text.Parsec (Parsec, ParseError, (<|>))
import qualified Text.Parsec as P
import Data.Text (Text)

type Parser = Parsec Text ()

parse :: Text -> Either ParseError Term
parse s = P.parse (pterm >>= \t -> P.eof >> return t) "recon" s

pterm :: Parser Term
pterm =
  P.try (P.chainl1 pterma $ pdelimiter >> return App) <|> pterma

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
                       , P.between (P.char '(') (P.lookAhead $ P.char ')') pterm
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
  pdelimiter
  t <- pterm
  return $ Succ t

ppred :: Parser Term
ppred = do
  _ <- P.string "pred"
  pdelimiter
  t <- pterm
  return $ Pred t

piszero :: Parser Term
piszero = do
  _ <- P.string "iszero"
  pdelimiter
  t <- pterm
  return $ IsZero t

pif :: Parser Term
pif = do
  _ <- P.string "if"
  pdelimiter
  t1 <- pterm
  pdelimiter
  _ <- P.string "then"
  pdelimiter
  t2 <- pterm
  pdelimiter
  _ <- P.string "else"
  pdelimiter
  t3 <- pterm
  return $ If t1 t2 t3

pabs :: Parser Term
pabs = do
  _ <- P.oneOf ['λ', '\\']
  pdelimiter
  x <- pvar
  pdelimiter
  _ <- P.char '.'
  pdelimiter
  t <- pterm
  return $ Abs x t

plet :: Parser Term
plet = do
  _ <- P.string "let"
  pdelimiter
  x <- pvar
  pdelimiter
  _ <- P.char '='
  pdelimiter
  t1 <- pterm
  pdelimiter
  _ <- P.string "in"
  pdelimiter
  t2 <- pterm
  return $ Let x t1 t2

pdelimiter :: Parser ()
pdelimiter = do
  P.try $ P.lookAhead (P.char '(' >> return ())
  <|> P.try (P.char ')' >> P.spaces)
  <|> P.skipMany1 P.space
