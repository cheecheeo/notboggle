{-# LANGUAGE ScopedTypeVariables #-}

module ListRandom where

import           Control.Monad
import           Data.Array.IO
import           System.Random

-- Property: length xs == length (shuffle xs)
-- http://en.wikipedia.org/wiki/Shuffling#Shuffling_algorithms
shuffle :: forall a. [a] -> IO [a]
-- shuffle xs = (pick . permutations) xs
{-shuffle xs = do let len = length xs
                withIndices <- liftM2 zip (randomTake len [0..(len-1)]) (return xs)
                return $ ((map snd) . (sortBy (\(index1, _) (index2, _) -> index1 `compare` index2))) withIndices-}
shuffle xs = let len = length xs in
             do array <- newListArray (0, len - 1) xs :: IO (IOArray Int a)
                swapTos <- swapTo (len-1)
                zipWithM_ (swap array) [0..] swapTos
                getElems array

swap :: IOArray Int a -> Int -> Int -> IO ()
swap array x y | x == y =
        return ()
               | otherwise =
        do a <- array `readArray` x
           b <- array `readArray` y
           writeArray array x b
           writeArray array y a

-- TODO: rename this function
swapTo :: Int -> IO [Int]
swapTo n = zipWithM (curry randomRIO) [0..n] (repeat n)

pick :: [a] -> IO a
pick xs = liftM2 (!!) (return xs) (randomRIO (0, length xs - 1))

randomTake :: Int -> [a] -> IO [a]
randomTake _ [] = return []
randomTake 0 _ = return []
randomTake n xs =
        do splitHere <- randomRIO (0, length xs - 1)
           let (left, choice : right) = splitAt splitHere xs
           liftM2 (:) (return choice) (randomTake (n-1) (left ++ right))
