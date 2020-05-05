module CleanStrike where

import Data.HashMap as Map
import Data.Hashable
import Data.Maybe

data NumberOfBlackCoins = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Show,Eq,Ord,Enum)

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
  Points Integer
  deriving (Show, Eq)

instance Monoid Points where
  mempty = Points 0

instance Semigroup Points where
  (<>) (Points p1) (Points p2) = Points (p1 + p2)

data PlayerPlay = PlayerPlay Player Play deriving (Show,Eq,Ord)

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
  deriving (Show, Eq,Ord)

incrementCountOfPlaysPerPlayer :: PlayerPlay -> Int -> Map PlayerPlay Int -> Map PlayerPlay Int
incrementCountOfPlaysPerPlayer = insertWith (+)

scoreBasedOnPlay :: Player -> Play -> Map PlayerPlay Int -> Map Player Points -> Map Player Points
scoreBasedOnPlay player play playerPerPlayCount = insertWith mappend player (pointsPerPlay player play playerPerPlayCount)


noStrikePoints :: Int -> Points
noStrikePoints totalNoStrikePlays =
  let foulCount = totalNoStrikePlays `div` 3
      remainderFoulCount = foulCount `div` 3
  in Points (-(toInteger(foulCount + remainderFoulCount)))

pointsPerPlay :: Player -> Play -> Map PlayerPlay Int -> Points
pointsPerPlay _ Strike      _  = Points 1
pointsPerPlay _ MultiStrike _  = Points 2
pointsPerPlay _ RedStrike   _  = Points 3
pointsPerPlay _ StrikerStrike _ = Points (-1)
pointsPerPlay _ DefunctCoin   _ = Points (-2)
pointsPerPlay p NoStrike playerPlayCount = noStrikePoints (findWithDefault 0 (PlayerPlay  p NoStrike) playerPlayCount)

coinsOutPerPlay :: Play -> Board -> Board
coinsOutPerPlay Strike (Board (BlackCoins num) r s) = Board (BlackCoins (pred num)) r s
coinsOutPerPlay MultiStrike (Board (BlackCoins num) r s) = Board (BlackCoins (pred (pred num))) r s
coinsOutPerPlay RedStrike (Board b _ s) = Board b Nothing s
coinsOutPerPlay DefunctCoin (Board (BlackCoins num) r s) = Board (BlackCoins (pred num)) r s
coinsOutPerPlay _ board = board

startPlay :: Play -> BoardState -> ([Play], BoardState)
startPlay play (BoardState board player playerPoints playerPlayCount) =
  let playerPlay = incrementCountOfPlaysPerPlayer (PlayerPlay player play) 1 playerPlayCount
      scoreCount = scoreBasedOnPlay player play playerPlay playerPoints
      newBoard = coinsOutPerPlay play board
      newPlayer = nextPlayer player
  in (validPlays newBoard ,BoardState newBoard newPlayer scoreCount playerPlay)

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

initialBoard :: Board
initialBoard = Board (BlackCoins Nine) (Just RedCoins) Striker

initialBoardState :: BoardState
initialBoardState = BoardState initialBoard Player1 Map.empty Map.empty

allPlays :: [Play]
allPlays =  [Strike,MultiStrike,RedStrike ,DefunctCoin ,StrikerStrike ,NoStrike ]


validPlays :: Board -> [Play]
validPlays (Board (BlackCoins num) redCoins _) | num >= Two = if (isJust redCoins) then allPlays else (Prelude.filter (/= RedStrike) allPlays )
validPlays (Board (BlackCoins num) redCoins _) | num == One = if (isJust redCoins) then Prelude.filter (/= MultiStrike ) allPlays else Prelude.filter (\play -> play /= MultiStrike || play /= RedStrike ) allPlays
