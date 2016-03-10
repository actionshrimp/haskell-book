{-# LANGUAGE OverloadedStrings #-}
module Scotty where

import Web.Scotty
import Control.Monad.Trans.Class

main :: IO ()
main = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    (lift :: IO a -> ActionM a) (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
