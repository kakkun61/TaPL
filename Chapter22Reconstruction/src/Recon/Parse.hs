module Recon.Parse (parse) where

import Recon.Type

import Text.Parsec (Parsec, ParseError, (<|>))
import qualified Text.Parsec as P
import Data.Text (Text)
import Debug.Trace (traceM)

type Parser = Parsec Text Int

parse :: Text -> Either ParseError Term
parse s =
  P.runParser go 0 "recon" s
  where
    go = do
      P.spaces
      t <- pterm
      P.spaces
      P.eof
      return t

pterm :: Parser Term
pterm = traceP "pterm" $
  P.chainl1 pterma $ P.try $ pspaces1 >> P.lookAhead pterma >> return App

pterma :: Parser Term
pterma = traceP "pterma" $ do
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
pvar = traceP "pvar" $ do
  fmap (ValueVarName . read) $
    P.try $ do
      h <- P.oneOf ['1' .. '9']
      tl <- P.many P.digit
      return $ h:tl
    <|> do
      z <- P.char '0'
      return [z]

pvart :: Parser Term
pvart = traceP "pvart" $ do
  x <- pvar
  return $ Var x

pzero :: Parser Term
pzero = traceP "pzero" $ do
  _ <- P.string "zero"
  return Zero

ptrue :: Parser Term
ptrue = traceP "ptrue" $ do
  _ <- P.string "true"
  return TTrue

pfalse :: Parser Term
pfalse = traceP "pfalse" $ do
  _ <- P.string "false"
  return TFalse

psucc :: Parser Term
psucc = traceP "psucc" $ do
  _ <- P.string "succ"
  pspaces1
  t <- pterm
  return $ Succ t

ppred :: Parser Term
ppred = traceP "ppred" $ do
  _ <- P.string "pred"
  pspaces1
  t <- pterm
  return $ Pred t

piszero :: Parser Term
piszero = traceP "piszero" $ do
  _ <- P.string "iszero"
  pspaces1
  t <- pterm
  return $ IsZero t

pif :: Parser Term
pif = traceP "pif" $ do
  _ <- P.string "if"
  pspaces1
  t1 <- pterm
  pspaces1
  _ <- P.string "then"
  pspaces1
  t2 <- pterm
  pspaces1
  _ <- P.string "else"
  pspaces1
  t3 <- pterm
  return $ If t1 t2 t3

pabs :: Parser Term
pabs = traceP "pabs" $ do
  _ <- P.oneOf ['λ', '\\']
  P.spaces
  x <- pvar
  P.spaces
  _ <- P.char '.'
  P.spaces
  t <- pterm
  return $ Abs x t

plet :: Parser Term
plet = traceP "plet" $ do
  _ <- P.string "let"
  pspaces1
  x <- pvar
  P.spaces
  _ <- P.char '='
  P.spaces
  t1 <- pterm
  pspaces1
  _ <- P.string "in"
  pspaces1
  t2 <- pterm
  return $ Let x t1 t2

pspaces1 :: Parser ()
pspaces1 =
  P.skipMany1 P.space

traceP :: String -> Parser a -> Parser a
traceP tag parser = do
  parserState <- P.getParserState
  let
    input = P.stateInput parserState
    depth = P.stateUser parserState
  traceM $ (replicate depth ' ') ++ tag ++ ": " ++ (show input)
  _ <- P.setParserState $ parserState { P.stateUser = depth + 1 }
  a <- parser
  _ <- P.updateParserState $ \s -> s { P.stateUser = depth }
  return a
