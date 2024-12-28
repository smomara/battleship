module Main where

import Game

-- Helper function to place initial ships for both players
setupInitialGame :: Either String Game
setupInitialGame = do
  g1 <- placeShipInGame createGame 1 Carrier (Coordinate 0 0) Horizontal
  g2 <- placeShipInGame g1 1 Battleship (Coordinate 0 2) Horizontal
  g3 <- placeShipInGame g2 1 Cruiser (Coordinate 0 4) Horizontal
  g4 <- placeShipInGame g3 1 Submarine (Coordinate 0 6) Horizontal
  g5 <- placeShipInGame g4 1 Destroyer (Coordinate 0 8) Horizontal

  g6 <- placeShipInGame g5 2 Carrier (Coordinate 0 0) Horizontal
  g7 <- placeShipInGame g6 2 Battleship (Coordinate 0 2) Horizontal
  g8 <- placeShipInGame g7 2 Cruiser (Coordinate 0 4) Horizontal
  g9 <- placeShipInGame g8 2 Submarine (Coordinate 0 6) Horizontal
  placeShipInGame g9 2 Destroyer (Coordinate 0 8) Horizontal

-- Simple demonstration of game mechanics
main :: IO ()
main = do
  putStrLn "Battleship Game Demonstration"
  case setupInitialGame of
    Left err -> putStrLn $ "Game setup failed: " ++ err
    Right initialGame -> do
      putStrLn "Initial game setup complete."
      putStrLn $ "Current Phase: " ++ show (phase initialGame)
      putStrLn $ "Current Player: " ++ show (currentPlayer initialGame)

      -- Demonstrate a few shots
      case fireShot initialGame (Coordinate 0 0) of
        Left err -> putStrLn $ "First shot failed: " ++ err
        Right game1 -> do
          putStrLn "First shot successful!"
          putStrLn $ "Current Player: " ++ show (currentPlayer game1)

          case fireShot game1 (Coordinate 1 0) of
            Left err -> putStrLn $ "Second shot failed: " ++ err
            Right game2 -> do
              putStrLn "Second shot successful!"
              putStrLn $ "Current Player: " ++ show (currentPlayer game2)
