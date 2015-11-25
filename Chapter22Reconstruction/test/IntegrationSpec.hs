{-# LANGUAGE OverloadedStrings #-}

module IntegrationSpec where

import Test.Hspec
import Recon
import Data.Bifunctor
import qualified Data.Map as M

spec :: Spec
spec = do
  describe "success" $ do
    it "\"let 0 = \\0. 0 in 0 true\" : Bool" $ do
      let
        script = "let 0 = \\0. 0 in 0 true"
      do
        term <- bimap show id $ parse script
        prinso (Context M.empty) term
      `shouldSatisfy` \a ->
        case a of
          Right (_, typ) -> typ == TBool
          Left _ -> False

    it "\"let 0 = \\0. 0 in 0 true[LF]\" : Bool" $ do
      let
        script = "let 0 = \\0. 0 in 0 true\n"
      do
        term <- bimap show id $ parse script
        prinso (Context M.empty) term
      `shouldSatisfy` \a ->
        case a of
          Right (_, typ) -> typ == TBool
          Left _ -> False

    it "\"let 0 = \\0. 0 in 0 true \" : Bool" $ do
      let
        script = "let 0 = \\0. 0 in 0 true "
      do
        term <- bimap show id $ parse script
        prinso (Context M.empty) term
      `shouldSatisfy` \a ->
        case a of
          Right (_, typ) -> typ == TBool
          Left _ -> False
