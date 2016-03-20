{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
    -- that's one, one click!
    -- two...two clicks!
    -- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

-- Stuff inside ScottyT is, except for things that escape
-- via IO, effectively read-only so we can't use StateT.
-- It would overcomplicate things to attempt to do so and
-- you should be using a proper database for production
-- applications.
type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (updated, incd) where
  incd = fromMaybe 0 (M.lookup k m) + 1
  updated = M.alter (const (Just incd)) k m

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    p <- lift $ asks prefix
    c <- lift $ asks counts
    let key' = mappend p unprefixed
    newInteger <- liftIO $ atomicModifyIORef' c (bumpBoomp key')
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newInteger
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runM v = runReaderT v config
  scottyT 3000 runM app
