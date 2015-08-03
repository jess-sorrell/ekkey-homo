{-#LANGUAGE OverloadedStrings #-}

module RandomTree where

import System.Random

import qualified Data.ByteString as B
import Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8


data Tree a = Leaf a | Node (Tree a) (Tree a)
data Binary = One | Zero deriving (Show, Eq)


randomTree :: (RandomGen g) => g -> [Binary] -> (Tree Binary, g)
randomTree gen [] = error "No value with which to make tree"
randomTree gen [x] = (Leaf x, gen)
randomTree gen xs =
  let (leftLen, g') = randomR (1, length xs) gen
  in
     let (leftTree, g'') = randomTree g' (take leftLen xs)
     in
      let (rightTree, g''') = randomTree g'' (drop leftLen xs)
      in (Node leftTree rightTree, g''')



toBinary :: B8.ByteString -> [Binary]
toBinary "" = []
toBinary xs
  | B8.head xs == '0' = [Zero, Zero, Zero, Zero] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == '1' = [Zero, Zero, Zero, One] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == '2' = [Zero, Zero, One, Zero] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == '3' = [Zero, Zero, One, One] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == '4' = [Zero, One, Zero, Zero] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == '5' = [Zero, One, Zero, One] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == '6' = [Zero, One, One, Zero] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == '7' = [Zero, One, One, One] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == '8' = [One, Zero, Zero, Zero] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == '9' = [One, Zero, Zero, One] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == 'a' = [One, Zero, One, Zero] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == 'b' = [One, Zero, One, One] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == 'c' = [One, One, Zero, Zero] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == 'd' = [One, One, Zero, One] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == 'e' = [One, One, One, Zero] ++ (toBinary $ B8.drop 1 xs)
  | B8.head xs == 'f' = [One, One, One, One] ++ (toBinary $ B8.drop 1 xs)
                        
