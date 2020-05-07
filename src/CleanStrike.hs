module CleanStrike where

import Data.HashMap as Map
import Data.Hashable
import Data.Maybe

data NumberOfBlackCoins
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Show, Eq, Ord, Enum)

newtype BlackCoin =
  BlackCoins NumberOfBlackCoins
  deriving (Show, Eq)

data RedCoin =
  RedCoins
  deriving (Show, Eq)

data Striker =
  Striker
  deriving (Show, Eq)

data Board =
  Board
    { blacks :: BlackCoin
    , red :: Maybe RedCoin
    , striker :: Striker
    }
  deriving (Show, Eq)

data Player
  = Player1
  | Player2
  deriving (Show, Eq, Ord, Enum)

instance Hashable Player where
  hashWithSalt i pl = hash (show pl)

newtype Points =
  Points
    { getPoints :: Integer
    }
  deriving (Show, Eq, Ord)

instance Num Points where
  (+) (Points p1) (Points p2) = Points (p1 + p2)
  (*) (Points p1) (Points p2) = Points (p1 * p2)
  abs (Points p) = Points (abs p)
  signum (Points p) = Points (signum p)
  fromInteger p = Points p
  (-) (Points p1) (Points p2) = Points (p1 - p2)

instance Monoid Points where
  mempty = Points 0

instance Semigroup Points where
  (<>) (Points p1) (Points p2) = Points (p1 + p2)

data PlayerPlay =
  PlayerPlay Player Play
  deriving (Show, Eq, Ord)

instance Hashable PlayerPlay where
  hashWithSalt _ (PlayerPlay a b) = hash (mappend (show a) (show b))

data BoardState =
  BoardState
    { board :: Board
    , turn :: Player
    , score :: Map Player Points
    , playsPerPlayer :: Map PlayerPlay Int
    }
  deriving (Show, Eq)

data Play
  = Strike
  | MultiStrike
  | RedStrike
  | StrikerStrike
  | DefunctCoin
  | NoStrike
  deriving (Show, Eq, Ord,Read)

data GameState
  = Draw String
  | Won String
  | Continue
  deriving (Show, Eq)

incrementCountOfPlaysPerPlayer ::
     PlayerPlay -> Int -> Map PlayerPlay Int -> Map PlayerPlay Int
incrementCountOfPlaysPerPlayer = insertWith (+)

scoreBasedOnPlay ::
     Player
  -> Play
  -> Map PlayerPlay Int
  -> Map Player Points
  -> Map Player Points
scoreBasedOnPlay player play playerPerPlayCount =
  insertWith mappend player (pointsPerPlay player play playerPerPlayCount)

noStrikePoints :: Int -> Points
noStrikePoints totalNoStrikePlays =
  let foulCount = totalNoStrikePlays `div` 3
      remainderFoulCount = foulCount `div` 3
   in Points (-(toInteger (foulCount + remainderFoulCount)))

pointsPerPlay :: Player -> Play -> Map PlayerPlay Int -> Points
pointsPerPlay _ Strike _ = Points 1
pointsPerPlay _ MultiStrike _ = Points 2
pointsPerPlay _ RedStrike _ = Points 3
pointsPerPlay _ StrikerStrike _ = Points (-1)
pointsPerPlay _ DefunctCoin _ = Points (-2)
pointsPerPlay p NoStrike playerPlayCount =
  noStrikePoints (findWithDefault 0 (PlayerPlay p NoStrike) playerPlayCount)

coinsOutPerPlay :: Play -> Board -> Board
coinsOutPerPlay Strike (Board (BlackCoins num) r s) =
  Board (BlackCoins (pred num)) r s
coinsOutPerPlay MultiStrike (Board (BlackCoins num) r s) =
  Board (BlackCoins (pred (pred num))) r s
coinsOutPerPlay RedStrike (Board b _ s) = Board b Nothing s
coinsOutPerPlay DefunctCoin (Board (BlackCoins num) r s) =
  Board (BlackCoins (pred num)) r s
coinsOutPerPlay _ board = board

initiateGame :: Play -> BoardState -> ([Play], BoardState)
initiateGame play (BoardState board player playerPoints playerPlayCount) =
  let playerPlay =
        incrementCountOfPlaysPerPlayer
          (PlayerPlay player play)
          1
          playerPlayCount
      scoreCount = scoreBasedOnPlay player play playerPlay playerPoints
      newBoard = coinsOutPerPlay play board
      newPlayer = nextPlayer player
   in (validPlays newBoard, BoardState newBoard newPlayer scoreCount playerPlay)

areWeThereYet :: BoardState -> GameState
areWeThereYet (BoardState (Board (BlackCoins Zero) Nothing Striker) _ _ _) =
  Draw "Game is a draw"
areWeThereYet (BoardState _ _ playerPoints _) =
  let player1Points = Map.findWithDefault (Points 0) Player1 playerPoints
      player2Points = Map.findWithDefault (Points 0) Player2 playerPoints
      diffPoints = player1Points - player2Points
      maxPoints = max player1Points player2Points
   in if maxPoints >= Points 5 && abs diffPoints >= Points 3
        then if signum diffPoints > 0
               then Won
                      ("Player1 wins the match, Score : Player 1 " ++
                       show (getPoints player1Points) ++
                       " Player 2 :" ++ show (getPoints player2Points))
               else Won
                      ("Player2 wins the match, Score : Player 1 " ++
                       show (getPoints player1Points) ++
                       " Player 2 :" ++ show (getPoints player2Points))
        else Continue

nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

initialBoard :: Board
initialBoard = Board (BlackCoins Nine) (Just RedCoins) Striker

initialBoardState :: BoardState
initialBoardState = BoardState initialBoard Player1 Map.empty Map.empty

allPlays :: [Play]
allPlays =
  [Strike, MultiStrike, RedStrike, DefunctCoin, StrikerStrike, NoStrike]

validPlays :: Board -> [Play]
validPlays (Board (BlackCoins num) redCoins _)
  | num >= Two =
    if isJust redCoins
      then allPlays
      else Prelude.filter (/= RedStrike) allPlays
validPlays (Board (BlackCoins num) redCoins _)
  | num == One =
    if isJust redCoins
      then Prelude.filter (/= MultiStrike) allPlays
      else Prelude.filter
             (\play -> play /= MultiStrike || play /= RedStrike)
             allPlays
