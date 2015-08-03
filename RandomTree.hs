{-#LANGUAGE OverloadedStrings #-}

module RandomTree where

import System.Random

import qualified Data.ByteString as B
import Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8


data Tree a = Leaf a | Node (Tree a) (Tree a)
data Binary = One | Zero deriving (Show, Eq)


randomTree :: (RandomGen g) => [Binary] -> g -> (Tree Binary, g)
randomTree [] gen = error "No value with which to make tree"
randomTree [x] gen = (Leaf x, gen)
randomTree xs gen =
  let (leftLen, gen') = randomR (1, length xs) gen
  in
     let (leftTree, gen'') = randomTree (take leftLen xs) gen'
     in
      let (rightTree, gen''') = randomTree (drop leftLen xs) gen''
      in (Node leftTree rightTree, gen''')



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
                        


toTree :: (RandomGen g) =>  B8.ByteString -> g -> (Tree Binary, g)
toTree msg gen = randomTree (toBinary (B16.encode msg)) gen
