module Main where

import Criterion.Main

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue e d) = Queue (a : e) d

pop :: Queue a -> (Maybe a, Queue a)
pop (Queue [] []) = (Nothing, empty)
pop (Queue [] (a : d)) = (Just a, Queue [] d)
pop (Queue e []) = (Just a, Queue [] r) where a : r = reverse e
pop (Queue e (a : d)) = (Just a, Queue [] (d ++ reverse e))


pushL :: a -> [a] -> [a]
pushL = (:)

popL :: [a] -> (Maybe a, [a])
popL [] = (Nothing, [])
popL xs = (Just (last xs), init xs)

usingList :: Int -> Int -> Int -> Int
usingList pushes pops n = last $ go n []
  where go 0 xs = xs
        go m xs = go (m - 1) (lPop pops (lPush 0 xs))
        lPush l xs | l == pushes = xs
                   | otherwise = lPush (l + 1) (pushL l xs)
        lPop 0 xs = snd . popL $ xs
        lPop k xs = lPop (k - 1) (snd . popL $ xs)

usingQueue :: Int -> Int -> Int -> Maybe Int
usingQueue pushes pops n = fst . pop $ go n empty
  where go 0 q = q
        go m q = go (m - 1) (qPop pops (qPush 0 q))
        qPush l q | l == pushes = q
                  | otherwise = qPush (l + 1) (push l q)
        qPop 0 q = snd . pop $ q
        qPop k q = qPop (k - 1) (snd . pop $ q)

main :: IO ()
main = defaultMain
  [ bench "altPushPop list" $ whnf (usingList 100 80) 100
  , bench "altPushPop queue" $ whnf (usingQueue 100 80) 100
  ]
