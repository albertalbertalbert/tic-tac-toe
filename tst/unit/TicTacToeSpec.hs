module TicTacToeSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import TicTacToe

spec :: Spec
spec = do 
  describe "TicTacToe.hs--win"  $ do
    it "This is a winning board" $ do
      (win win1 X `shouldBe` (X,True)) 
    it "This is not a winning board" $ do
          (win win1 O `shouldBe` (O,False))

win1 = [X,O,O
        ,B,X,O
        ,B,X,X]
