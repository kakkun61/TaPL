module Main where

import Recon

import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Text.IO as T

-- TODO Get context of expression
main :: IO ()
main = do
  script <- T.getContents
  let
    result = do
      term <- bimap show id $ parse script
      prinso (Context M.empty) term
  putStrLn $ case result of
    Right (_, typ) -> show typ
    Left err -> "failed: " ++ err
