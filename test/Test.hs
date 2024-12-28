module Main where

import Data.Map qualified as M
import Game
import Test.HUnit

-- Basic Game Creation Tests
testCreateGame :: Test
testCreateGame =
  TestList
    [ "New game should be in Setup phase"
        ~: Setup
        @=? phase createGame
    , "New game should start with player 1"
        ~: 1
        @=? currentPlayer createGame
    , "New game should have empty boards"
        ~: M.empty
        @=? ships (player1 createGame)
    ]

-- Ship Placement Tests
testShipPlacement :: Test
testShipPlacement =
  TestList
    [ "Should place ship successfully"
        ~: TestCase
        $ case placeShipInGame createGame 1 Carrier (Coordinate 0 0) Horizontal of
          Right game -> assertBool "Ship should be placed" $ M.member Carrier (ships $ player1 game)
          Left err -> assertFailure $ "Ship placement failed: " ++ err
    , "Should fail placing ship out of bounds"
        ~: TestCase
        $ case placeShipInGame createGame 1 Carrier (Coordinate 8 0) Horizontal of
          Left _ -> return ()
          Right _ -> assertFailure "Ship placement should have failed"
    , "Should fail placing same ship twice"
        ~: TestCase
        $ do
          let result = do
                game <- placeShipInGame createGame 1 Carrier (Coordinate 0 0) Horizontal
                placeShipInGame game 1 Carrier (Coordinate 0 2) Horizontal
          case result of
            Left _ -> return ()
            Right _ -> assertFailure "Should not allow placing same ship twice"
    ]

-- Helper to set up game in Playing phase
setupGameForPlay :: Either String Game
setupGameForPlay = do
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

-- Shot Tests
testShots :: Test
testShots =
  TestList
    [ "Players should alternate turns"
        ~: TestCase
        $ case setupGameForPlay of
          Right game -> do
            -- Player 1's turn
            case fireShot game (Coordinate 0 0) of
              Right game1 -> do
                assertEqual "Should be player 2's turn" 2 (currentPlayer game1)
                -- Player 2's turn
                case fireShot game1 (Coordinate 1 0) of
                  Right game2 -> assertEqual "Should be player 1's turn" 1 (currentPlayer game2)
                  Left err -> assertFailure $ "Player 2 shot failed: " ++ err
              Left err -> assertFailure $ "Player 1 shot failed: " ++ err
          Left err -> assertFailure $ "Setup failed: " ++ err
    , "Same player cannot shoot same spot twice"
        ~: TestCase
        $ case setupGameForPlay of
          Right game -> do
            -- Player 1 shoots at 0,0
            case fireShot game (Coordinate 0 0) of
              Right game1 -> do
                -- Player 2 shoots somewhere else
                case fireShot game1 (Coordinate 1 0) of
                  Right game2 -> do
                    -- Player 1 tries to shoot 0,0 again
                    case fireShot game2 (Coordinate 0 0) of
                      Left _ -> return ()
                      Right _ -> assertFailure "Should not allow same player to shoot same spot"
                  Left err -> assertFailure $ "Player 2 shot failed: " ++ err
              Left err -> assertFailure $ "Player 1 shot failed: " ++ err
          Left err -> assertFailure $ "Setup failed: " ++ err
    , "Different players can shoot same spot"
        ~: TestCase
        $ case setupGameForPlay of
          Right game -> do
            -- Player 1 shoots at 0,0
            case fireShot game (Coordinate 0 0) of
              Right game1 -> do
                -- Player 2 tries to shoot at 0,0 (should be allowed)
                case fireShot game1 (Coordinate 0 0) of
                  Right _ -> return ()
                  Left err -> assertFailure $ "Player 2 should be able to shoot same spot: " ++ err
              Left err -> assertFailure $ "Player 1 shot failed: " ++ err
          Left err -> assertFailure $ "Setup failed: " ++ err
    ]

-- Process shots in sequence
shotSequence :: Game -> [Coordinate] -> Either String Game
shotSequence game [] = Right game
shotSequence game (coord : rest) = case fireShot game coord of
  Left err -> Left $ "Shot failed at " ++ show coord ++ ": " ++ err
  Right newGame -> shotSequence newGame rest

-- Game Phase Tests
testGamePhases :: Test
testGamePhases =
  TestList
    [ "Game should move to Playing after all ships placed"
        ~: TestCase
        $ case setupGameForPlay of
          Right game -> assertEqual "Phase should be Playing" Playing (phase game)
          Left err -> assertFailure $ "Failed to place ships: " ++ err
    , "Game should end when all ships sunk"
        ~: TestCase
        $ case sinkAllShipsTest of
          Right game -> case phase game of
            GameOver _ -> return ()
            _ -> assertFailure $ "Game should be over, but phase is " ++ show (phase game)
          Left err -> assertFailure $ "Failed to process shots: " ++ err
    ]

-- Test for sinking all ships
sinkAllShipsTest :: Either String Game
sinkAllShipsTest = do
  game <- setupGameForPlay
  -- Player 1 will sink all of Player 2's ships
  -- Player 2 will take alternate shots at different locations
  shotSequence
    game
    [ -- Sink Player 2's Carrier (at 0,0 to 5,0)
      Coordinate 0 0   -- P1 shot at Carrier
    , Coordinate 9 0   -- P2 dummy shot
    , Coordinate 1 0   -- P1 shot at Carrier
    , Coordinate 9 1   -- P2 dummy shot
    , Coordinate 2 0   -- P1 shot at Carrier
    , Coordinate 9 2   -- P2 dummy shot
    , Coordinate 3 0   -- P1 shot at Carrier
    , Coordinate 9 3   -- P2 dummy shot
    , Coordinate 4 0   -- P1 shot at Carrier
    , Coordinate 9 4   -- P2 dummy shot
    , Coordinate 5 0   -- P1 shot at Carrier
    , Coordinate 9 5   -- P2 dummy shot

      -- Sink Player 2's Battleship (at 0,2 to 4,2)
    , Coordinate 0 2   -- P1 shot at Battleship
    , Coordinate 9 6   -- P2 dummy shot
    , Coordinate 1 2   -- P1 shot at Battleship
    , Coordinate 9 7   -- P2 dummy shot
    , Coordinate 2 2   -- P1 shot at Battleship
    , Coordinate 9 8   -- P2 dummy shot
    , Coordinate 3 2   -- P1 shot at Battleship
    , Coordinate 9 9   -- P2 dummy shot
    , Coordinate 4 2   -- P1 shot at Battleship
    , Coordinate 8 0   -- P2 dummy shot

      -- Sink Player 2's Cruiser (at 0,4 to 3,4)
    , Coordinate 0 4   -- P1 shot at Cruiser
    , Coordinate 8 1   -- P2 dummy shot
    , Coordinate 1 4   -- P1 shot at Cruiser
    , Coordinate 8 2   -- P2 dummy shot
    , Coordinate 2 4   -- P1 shot at Cruiser
    , Coordinate 8 3   -- P2 dummy shot
    , Coordinate 3 4   -- P1 shot at Cruiser
    , Coordinate 8 4   -- P2 dummy shot

      -- Sink Player 2's Submarine (at 0,6 to 3,6)
    , Coordinate 0 6   -- P1 shot at Submarine
    , Coordinate 8 5   -- P2 dummy shot
    , Coordinate 1 6   -- P1 shot at Submarine
    , Coordinate 8 6   -- P2 dummy shot
    , Coordinate 2 6   -- P1 shot at Submarine
    , Coordinate 8 7   -- P2 dummy shot
    , Coordinate 3 6   -- P1 shot at Submarine
    , Coordinate 8 8   -- P2 dummy shot

      -- Sink Player 2's Destroyer (at 0,8 to 2,8)
    , Coordinate 0 8   -- P1 shot at Destroyer
    , Coordinate 8 9   -- P2 dummy shot
    , Coordinate 1 8   -- P1 shot at Destroyer, all ships sunk
    ]

main :: IO ()
main = do
  putStrLn "Running Battleship Tests..."
  counts <-
    runTestTT $
      TestList
        [ "Game Creation" ~: testCreateGame
        , "Ship Placement" ~: testShipPlacement
        , "Shots" ~: testShots
        , "Game Phases" ~: testGamePhases
        ]
  if failures counts > 0 then error "Tests failed!" else return ()
