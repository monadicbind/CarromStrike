module CleanStrikeSpec where

import CleanStrike
import Test.Hspec
import Data.HashMap as Map

inputBoard :: Board
inputBoard = Board (BlackCoins One) Nothing Striker

inputScore :: Map Player PointsAndCount
inputScore = empty

initialBoardState :: BoardState
initialBoardState = BoardState inputBoard Player1 inputScore

testCondition :: Play -> BoardState -> Either String Points
testCondition play boardState = fmap (points . Map.findWithDefault (PointsAndCount 0 (Points 0)) Player1 . score) (makeAPlay play boardState)

spec :: Spec
spec = do
  describe "Given Strike Play on a Board with One Black" $ do
    it "should return an Empty Board" $ do
      (testCondition Strike initialBoardState) `shouldBe` (Right (Points 1))
