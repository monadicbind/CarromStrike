module Main where

import CleanStrike
import Control.Monad.Loops
import Lib

main :: IO ()
main = do
  p@(plays , bs) <- displayLoop (allPlays , initialBoardState )
  let s = areWeThereYet bs
  print s

displayLoop :: ([Play],BoardState) -> IO ([Play],BoardState)
displayLoop (plays,bs) = do
  _ <- print (turn bs)
  _ <- print plays
  play <- getLine
  p@(newPlays ,newBS) <- pure (initiateGame (read play) bs)
  if keepGoing p then displayLoop p else pure p

keepGoing :: ([Play],BoardState) -> Bool
keepGoing (plays , bs) = keepGoingHelper (areWeThereYet bs)
  where
    keepGoingHelper (Won str) = False
    keepGoingHelper (Draw str) = False
    keepGoingHelper Continue = True

