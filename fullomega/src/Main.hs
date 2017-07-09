module Main where

import qualified Data.Text.IO as T

import SystemFOmega

main :: IO ()
main = do
  script <- T.getContents
  let ast = parse script
  putStrLn $ show ast
