module SystemFOmega.Parse (parse) where

import SystemFOmega.Type

import Text.Parsec (Parsec, ParseError, (<|>))
import qualified Text.Parsec as P
import Data.Text (Text)

type Parser = Parsec Text ()

parse :: Text -> Either ParseError Term
parse s =
  P.parse go "recon" s
  where
    go = do
      P.spaces
      t <- pterm
      P.spaces
      P.eof
      return t

pterm :: Parser Term
pterm =
  P.chainl1 pterma $ P.try $ pspaces1 >> P.lookAhead pterma >> return App

pterma :: Parser Term
pterma =
  P.choice $ map P.try [ pvart
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
  pspaces1
  t <- pterm
  return $ Succ t

ppred :: Parser Term
ppred = do
  _ <- P.string "pred"
  pspaces1
  t <- pterm
  return $ Pred t

piszero :: Parser Term
piszero = do
  _ <- P.string "iszero"
  pspaces1
  t <- pterm
  return $ IsZero t

pif :: Parser Term
pif = do
  _ <- P.string keywordIf
  pspaces1
  t1 <- pterm
  pspaces1
  _ <- P.string keywordThen
  pspaces1
  t2 <- pterm
  pspaces1
  _ <- P.string keywordElse
  pspaces1
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

plet :: Parser Term
plet = do
  _ <- P.string keywordLet
  pspaces1
  x <- pvar
  P.spaces
  _ <- P.char '='
  P.spaces
  t1 <- pterm
  pspaces1
  _ <- P.string keywordIn
  pspaces1
  t2 <- pterm
  return $ Let x t1 t2

pspaces1 :: Parser ()
pspaces1 =
  P.skipMany1 P.space
