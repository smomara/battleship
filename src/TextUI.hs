module TextUI where

import Data.Map qualified as M
import Game
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- | Clear the screen using ANSI escape codes
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- | Display a board with proper formatting
displayBoard :: PlayerState -> Bool -> IO ()
displayBoard state showShips = do
  putStrLn "     0   1   2   3   4   5   6   7   8   9"
  putStrLn "   +---+---+---+---+---+---+---+---+---+---+"
  mapM_
    ( \y -> do
        displayRow state showShips y
        putStrLn "   +---+---+---+---+---+---+---+---+---+---+"
    )
    [0 .. boardSize - 1]

displayRow :: PlayerState -> Bool -> Int -> IO ()
displayRow state showShips y = do
  putStr $ " " ++ show y ++ " |"
  mapM_ (\x -> putStr $ cellChar state (Coordinate x y) showShips ++ "|") [0 .. boardSize - 1]
  putStrLn ""

-- | Convert a cell to its display character
cellChar :: PlayerState -> Coordinate -> Bool -> String
cellChar state coord showShips =
  case getCellStatus state coord of
    Empty -> " · "
    Ship st -> if showShips then " " ++ shipTypeChar st ++ " " else " · "
    Hit _ -> " X "
    Miss -> " O "

-- | Convert ship type to character representation
shipTypeChar :: ShipType -> String
shipTypeChar Carrier = "C"
shipTypeChar Battleship = "B"
shipTypeChar Cruiser = "R"
shipTypeChar Submarine = "S"
shipTypeChar Destroyer = "D"

-- | Read coordinates from user input
readCoordinate :: String -> IO Coordinate
readCoordinate prompt = do
  putStr $ prompt ++ " (x y): "
  hFlush stdout
  input <- getLine
  case words input of
    [xs, ys] -> case (readMaybe xs, readMaybe ys) of
      (Just x, Just y) ->
        if inBounds (Coordinate x y)
          then return $ Coordinate x y
          else putStrLn "Coordinates out of bounds!" >> readCoordinate prompt
      _ -> putStrLn "Invalid input!" >> readCoordinate prompt
    _ -> putStrLn "Please enter two numbers!" >> readCoordinate prompt

-- | Read orientation from user input
readOrientation :: IO Orientation
readOrientation = do
  putStr "Enter orientation (h/v): "
  hFlush stdout
  input <- getLine
  case input of
    "h" -> return Horizontal
    "v" -> return Vertical
    _ -> putStrLn "Please enter 'h' or 'v'!" >> readOrientation

-- | Place ships phase
placeShips :: Game -> Int -> IO Game
placeShips game playerNum = do
  clearScreen
  putStrLn $ "Player " ++ show playerNum ++ ", place your ships!"
  displayBoard (if playerNum == 1 then player1 game else player2 game) True

  let remainingShips =
        filter
          ( \st ->
              not $
                M.member st $
                  ships $
                    if playerNum == 1 then player1 game else player2 game
          )
          allShipTypes

  if null remainingShips
    then return game
    else do
      let ship = head remainingShips
      putStrLn $ "\nPlacing " ++ show ship ++ " (length: " ++ show (shipLength ship) ++ ")"
      coord <- readCoordinate "Enter starting coordinate"
      orientation <- readOrientation

      case placeShipInGame game playerNum ship coord orientation of
        Left err -> do
          putStrLn $ "\nError: " ++ err
          putStrLn "Press Enter to try again..."
          _ <- getLine
          placeShips game playerNum
        Right newGame -> placeShips newGame playerNum

-- | Main game loop for shooting phase
playGame :: Game -> IO Game
playGame game = do
  clearScreen
  let currentPlayerNum = currentPlayer game
      currentPlayerState = if currentPlayerNum == 1 then player1 game else player2 game
      opponentState = if currentPlayerNum == 1 then player2 game else player1 game

  putStrLn $ "Player " ++ show currentPlayerNum ++ "'s turn"
  putStrLn "\nYour board:"
  displayBoard currentPlayerState True
  putStrLn "\nOpponent's board:"
  displayBoard opponentState False

  coord <- readCoordinate "Enter coordinate to fire at"
  case fireShot game coord of
    Left err -> do
      putStrLn $ "\nError: " ++ err
      putStrLn "Press Enter to try again..."
      _ <- getLine
      playGame game
    Right newGame -> do
      let newOpponentState = if currentPlayer game == 1 then player2 newGame else player1 newGame
          hit = case getCellStatus newOpponentState coord of
            Hit _ -> True
            _ -> False
      putStrLn $ if hit then "Hit!" else "Miss!"

      -- Check for newly sunk ships
      let oldSunkShips = filter (isShipSunk opponentState) allShipTypes
          newSunkShips = filter (isShipSunk newOpponentState) allShipTypes
          newlySunkShips = filter (`notElem` oldSunkShips) newSunkShips

      mapM_ (\ship -> putStrLn $ "You have sunk the " ++ show ship ++ "!") newlySunkShips
      putStrLn "Press Enter to continue..."
      _ <- getLine
      case phase newGame of
        GameOver winner -> do
          clearScreen
          putStrLn $
            "Game Over! "
              ++ case winner of
                Player1Wins -> "Player 1 wins!"
                Player2Wins -> "Player 2 wins!"
          return newGame
        _ -> playGame newGame

-- | Main game runner
runGame :: IO ()
runGame = do
  clearScreen
  putStrLn "Welcome to Battleship!"
  putStrLn "\nPlayer 1, press Enter to place your ships..."
  _ <- getLine

  game1 <- placeShips createGame 1

  clearScreen
  putStrLn "Player 2, press Enter to place your ships..."
  _ <- getLine

  game2 <- placeShips game1 2

  case phase game2 of
    Playing -> do
      clearScreen
      putStrLn "All ships placed! Press Enter to start the game..."
      _ <- getLine
      _ <- playGame game2
      return ()
    _ -> putStrLn "Error: Game didn't transition to Playing phase!"
