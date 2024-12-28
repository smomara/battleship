module Game where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

data Coordinate = Coordinate
  { x :: Int
  , y :: Int
  }
  deriving (Show, Eq, Ord)

data ShipType = Carrier | Battleship | Cruiser | Submarine | Destroyer
  deriving (Show, Eq, Ord)

shipLength :: ShipType -> Int
shipLength Carrier = 5
shipLength Battleship = 4
shipLength Cruiser = 3
shipLength Submarine = 3
shipLength Destroyer = 2

-- All ship types in standard order
allShipTypes :: [ShipType]
allShipTypes = [Carrier, Battleship, Cruiser, Submarine, Destroyer]

data Orientation = Horizontal | Vertical
  deriving (Show, Eq)

data Cell
  = Empty
  | Ship ShipType -- Ship on your board
  | Hit ShipType -- Hit on a ship (shows which ship was hit)
  | Miss -- Shot that missed
  deriving (Show, Eq)

type Board = Map Coordinate Cell

data PlayerState = PlayerState
  { board :: Board -- Player's own board with their ships
  , shots :: Set Coordinate -- Where they've fired at opponent
  , ships :: Map ShipType (Set Coordinate) -- Where each ship is located
  , hits :: Set Coordinate -- Hits received on their ships
  }
  deriving (Show, Eq)

data Phase = Setup | Playing | GameOver Winner
  deriving (Show, Eq)

data Winner = Player1Wins | Player2Wins
  deriving (Show, Eq)

data Game = Game
  { player1 :: PlayerState
  , player2 :: PlayerState
  , currentPlayer :: Int -- 1 or 2
  , phase :: Phase
  }
  deriving (Show)

boardSize :: Int
boardSize = 10

createEmptyBoard :: Board
createEmptyBoard =
  M.fromList
    [(Coordinate x y, Empty) | x <- [0 .. boardSize - 1], y <- [0 .. boardSize - 1]]

createPlayerState :: PlayerState
createPlayerState =
  PlayerState
    { board = createEmptyBoard
    , shots = S.empty
    , ships = M.empty
    , hits = S.empty
    }

createGame :: Game
createGame =
  Game
    { player1 = createPlayerState
    , player2 = createPlayerState
    , currentPlayer = 1
    , phase = Setup
    }

inBounds :: Coordinate -> Bool
inBounds (Coordinate x y) = x >= 0 && x < boardSize && y >= 0 && y < boardSize

shipCoordinates :: Coordinate -> ShipType -> Orientation -> [Coordinate]
shipCoordinates (Coordinate x y) ship orientation = case orientation of
  Horizontal -> [Coordinate (x + i) y | i <- [0 .. shipLength ship - 1]]
  Vertical -> [Coordinate x (y + i) | i <- [0 .. shipLength ship - 1]]

placeShip :: PlayerState -> ShipType -> Coordinate -> Orientation -> Either String PlayerState
placeShip player shipType start orientation
  | M.member shipType (ships player) =
      Left "Ship already placed"
  | not (all inBounds coords) =
      Left "Ship placement out of bounds"
  | any (hasShip (board player)) coords =
      Left "Space already occupied by another ship"
  | otherwise =
      Right $
        player
          { board = foldr (\c b -> M.insert c (Ship shipType) b) (board player) coords
          , ships = M.insert shipType (S.fromList coords) (ships player)
          }
 where
  coords = shipCoordinates start shipType orientation
  hasShip b c = case M.lookup c b of
    Just (Ship _) -> True
    _ -> False

placeShipInGame :: Game -> Int -> ShipType -> Coordinate -> Orientation -> Either String Game
placeShipInGame game playerNum shipType start orientation
  | phase game /= Setup =
      Left "Can only place ships during setup phase"
  | playerNum /= 1 && playerNum /= 2 =
      Left "Invalid player number"
  | otherwise = case placeShip playerState shipType start orientation of
      Left err -> Left err
      Right newState -> Right $ checkPhase $ updatePlayer game playerNum newState
 where
  playerState = if playerNum == 1 then player1 game else player2 game

fireShot :: Game -> Coordinate -> Either String Game
fireShot game coord
  | phase game /= Playing =
      Left "Game is not in playing phase"
  | not (inBounds coord) =
      Left "Shot out of bounds"
  | coord `S.member` shots shooter =
      Left "Already fired at this coordinate"
  | otherwise =
      Right $
        checkPhase $
          game
            { player1 = if isPlayer1 then newShooter else newTarget
            , player2 = if isPlayer1 then newTarget else newShooter
            , currentPlayer = nextPlayer
            }
 where
  isPlayer1 = currentPlayer game == 1
  nextPlayer = if isPlayer1 then 2 else 1
  shooter = if isPlayer1 then player1 game else player2 game
  target = if isPlayer1 then player2 game else player1 game

  -- Find any ship at the coordinate
  hitShip =
    M.foldrWithKey
      ( \sType coords mShip ->
          if coord `S.member` coords
            then Just sType
            else mShip
      )
      Nothing
      (ships target)

  -- Update shooter's state (record their shot)
  newShooter = shooter{shots = S.insert coord (shots shooter)}

  -- Update target's state (record hit if any)
  newTarget = case hitShip of
    Just shipType ->
      target
        { board = M.insert coord (Hit shipType) (board target)
        , hits = S.insert coord (hits target)
        }
    Nothing ->
      target
        { board = M.insert coord Miss (board target)
        }

updatePlayer :: Game -> Int -> PlayerState -> Game
updatePlayer game 1 newState = game{player1 = newState}
updatePlayer game 2 newState = game{player2 = newState}
updatePlayer game _ _ = game

isSetupComplete :: PlayerState -> Bool
isSetupComplete player = M.size (ships player) == 5 -- All ships placed

isShipSunk :: PlayerState -> ShipType -> Bool
isShipSunk player shipType = case M.lookup shipType (ships player) of
  Just coords -> all (`S.member` hits player) coords
  Nothing -> False

areAllShipsSunk :: PlayerState -> Bool
areAllShipsSunk player = all (isShipSunk player) allShipTypes

checkPhase :: Game -> Game
checkPhase game
  | phase game == Setup && setupDone =
      game{phase = Playing}
  | phase game == Playing && gameOver =
      game{phase = GameOver winner}
  | otherwise = game
 where
  setupDone = isSetupComplete (player1 game) && isSetupComplete (player2 game)
  gameOver = areAllShipsSunk (player1 game) || areAllShipsSunk (player2 game)
  winner
    | areAllShipsSunk (player1 game) = Player2Wins
    | areAllShipsSunk (player2 game) = Player1Wins
    | otherwise = error "Game is not over"

getCellStatus :: PlayerState -> Coordinate -> Cell
getCellStatus player coord = M.findWithDefault Empty coord (board player)
