module Test.Main where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (Cell(..), Player(..), anyMovesLeft, checkWinner, isGameOver, mkBoard)
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  let unsafeMkBoard cells = unsafePartial $ fromJust $ mkBoard cells
  describe "checkWinner" do
    it "empty" do
      let
        board = unsafeMkBoard
          [ [ Empty, Empty ]
          , [ Empty, Empty ]
          ]
      checkWinner board `shouldEqual` Nothing
    it "x row" do
      let
        board = unsafeMkBoard
          [ [ Empty, Empty ]
          , [ XSymbol, XSymbol ]
          ]
      checkWinner board `shouldEqual` Just XPlayer
    it "o column" do
      let
        board = unsafeMkBoard
          [ [ Empty, OSymbol ]
          , [ XSymbol, OSymbol ]
          ]
      checkWinner board `shouldEqual` Just OPlayer
    it "x diagonal" do
      let
        board = unsafeMkBoard
          [ [ XSymbol, Empty ]
          , [ OSymbol, XSymbol ]
          ]
      checkWinner board `shouldEqual` Just XPlayer
    it "o 2x2" do
      let
        board = unsafeMkBoard
          [ [ XSymbol, Empty, XSymbol ]
          , [ OSymbol, OSymbol, Empty ]
          , [ OSymbol, OSymbol, XSymbol ]
          ]
      checkWinner board `shouldEqual` Just OPlayer
    it "x 4 corners" do
      let
        board = unsafeMkBoard
          [ [ XSymbol, Empty, XSymbol ]
          , [ OSymbol, OSymbol, Empty ]
          , [ XSymbol, OSymbol, XSymbol ]
          ]
      checkWinner board `shouldEqual` Just XPlayer
    it "real game" do
      let
        board = unsafeMkBoard
          [ [ OSymbol, XSymbol, OSymbol, OSymbol ]
          , [ OSymbol, XSymbol, XSymbol, Empty ]
          , [ Empty, XSymbol, Empty, Empty ]
          , [ XSymbol, XSymbol, Empty, OSymbol ]
          ]
      checkWinner board `shouldEqual` Just XPlayer
    it "real game 2" do
      let
        board = unsafeMkBoard
          [ [ OSymbol, XSymbol, OSymbol, OSymbol ]
          , [ OSymbol, XSymbol, XSymbol, XSymbol ]
          , [ OSymbol, OSymbol, XSymbol, OSymbol ]
          , [ XSymbol, XSymbol, OSymbol, XSymbol ]
          ]
      checkWinner board `shouldEqual` Nothing

  describe "anyMovesLeft" do
    it "true" do
      let
        board = unsafeMkBoard
          [ [ OSymbol, XSymbol, OSymbol, OSymbol ]
          , [ OSymbol, XSymbol, XSymbol, XSymbol ]
          , [ OSymbol, OSymbol, XSymbol, OSymbol ]
          , [ XSymbol, XSymbol, OSymbol, Empty ]
          ]
      anyMovesLeft board `shouldEqual` true
    it "false" do
      let
        board = unsafeMkBoard
          [ [ OSymbol, XSymbol, OSymbol, OSymbol ]
          , [ OSymbol, XSymbol, XSymbol, XSymbol ]
          , [ OSymbol, OSymbol, XSymbol, OSymbol ]
          , [ XSymbol, XSymbol, OSymbol, XSymbol ]
          ]
      anyMovesLeft board `shouldEqual` false
  describe "isGameOver" do
    it "no moves left" do
      let
        board = unsafeMkBoard
          [ [ OSymbol, XSymbol, OSymbol, OSymbol ]
          , [ OSymbol, XSymbol, XSymbol, XSymbol ]
          , [ OSymbol, OSymbol, XSymbol, OSymbol ]
          , [ XSymbol, XSymbol, OSymbol, XSymbol ]
          ]
      isGameOver board `shouldEqual` true
    it "someone won" do
      let
        board = unsafeMkBoard
          [ [ OSymbol, XSymbol, OSymbol, OSymbol ]
          , [ OSymbol, XSymbol, XSymbol, XSymbol ]
          , [ OSymbol, OSymbol, XSymbol, OSymbol ]
          , [ OSymbol, XSymbol, OSymbol, Empty ]
          ]
      isGameOver board `shouldEqual` true
    it "game not over" do
      let
        board = unsafeMkBoard
          [ [ OSymbol, XSymbol, OSymbol, OSymbol ]
          , [ OSymbol, XSymbol, XSymbol, XSymbol ]
          , [ OSymbol, OSymbol, XSymbol, OSymbol ]
          , [ XSymbol, XSymbol, OSymbol, Empty ]
          ]
      isGameOver board `shouldEqual` false