{-# LANGUAGE OverloadedStrings #-}

module Mora where

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Text.Read (readMaybe)

data Player = Player1 | Player2

data MoraGameState =
  MoraGameState { p1Score :: Integer
                , p2Score :: Integer
                }

type GameTurn = (Integer, Integer)

type MoraGame =
  ExceptT String (StateT MoraGameState IO)

playerLabel :: Player -> Text
playerLabel Player1 = "Player1: "
playerLabel Player2 = "Player2: "

turnQuestion :: Player -> Text -> IO Integer
turnQuestion p q = do
  putStrLn . TL.unpack $ playerLabel p `mappend` q
  v <- getLine
  case readMaybe v of
    Nothing -> do
      putStrLn "Didn't recognise that!"
      turnQuestion p q
    Just parsed -> return parsed

gameTurn :: Player -> StateT MoraGameState IO (Integer, Integer)
gameTurn p = do
  fingers <- liftIO $ turnQuestion p "Your fingers?"
  guess <- liftIO $ turnQuestion p "Your guess for the total?"
  return (fingers, guess)

updateScores :: GameTurn -> GameTurn -> StateT MoraGameState IO (Integer, Integer)
updateScores (p1Fingers, p1Guess) (p2Fingers, p2Guess) = do
  s <- get
  let totalFingers = p1Fingers + p2Fingers
      MoraGameState p1s p2s = s
      p1Guessed = p1Guess == totalFingers
      p2Guessed = p2Guess == totalFingers
      p1NewScore = if p1Guessed then p1s + 1 else p1s
      p2NewScore = if p2Guessed then p2s + 1 else p2s
      s' = s { p1Score = p1NewScore
             , p2Score = p2NewScore
             }
  put s'
  when p1Guessed (liftIO (putStrLn "Player1 gets a point!"))
  when p2Guessed (liftIO (putStrLn "Player2 gets a point!"))
  return (p1NewScore, p2NewScore)

gameLoop :: StateT MoraGameState IO ()
gameLoop = do
  p1Turn <- gameTurn Player1
  p2Turn <- gameTurn Player2
  scores <- updateScores p1Turn p2Turn
  case scores of
    (3, 3) -> liftIO (putStrLn "It's a tie!")
    (3, _) -> liftIO (putStrLn "Player 1 wins!")
    (_, 3) -> liftIO (putStrLn "Player 2 wins!")
    _ -> gameLoop

main :: IO ()
main = do
  let initialState = MoraGameState 0 0
  _ <- runStateT gameLoop initialState
  return ()
