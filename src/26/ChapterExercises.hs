{-# LANGUAGE InstanceSigs #-}
module ChapterExercises where

import Data.Functor.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

rDec :: Num a => Reader a a
rDec = fmap (subtract 1) ask

rShow :: Show a => ReaderT a Identity String
rShow = fmap show ask

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  v <- ask
  lift . putStrLn $ "Hi: " ++ show v
  return $ v + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  v <- get
  lift . putStrLn $ "Hi: " ++ show v
  put (v + 1)
  return (show v)
