{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Gadget where

import Data.Reflection
import Data.Proxy
import Text.Printf
import System.Random
import Zn
import Poly



go_go_gadget_elem :: (Integral a) => a -> [a]
go_go_gadget_elem 0 = [0]
go_go_gadget_elem 1 = [1]
go_go_gadget_elem x = (go_go_gadget_elem $ x `quot` 2)++[(x `mod` 2)]



go_go_gadget_inverse' :: (Integral a) => [a] -> [[a]]
go_go_gadget_inverse' as
  | (filter ( <= 1) as) == as = [as]
  | otherwise = (map (\x -> x `mod` 2) as):(go_go_gadget_inverse' $ map (\x -> x `quot` 2) as) 


go_go_gadget_inverse :: (Integral a) => a -> [a] -> [[a]]
go_go_gadget_inverse modulus as =
  let expand = go_go_gadget_inverse' $ map (\x -> x `mod` modulus ) as
  in
   let pad = (floor $ logBase 2 $ fromIntegral modulus) - (length expand)
   in
    expand++([ [0 | i <- [0..((length as) - 1)]] | j <- [0..pad]])


go_go_gadget_invector :: (Integral a) => a -> [[a]] -> [[[a]]]
go_go_gadget_invector modulus vec = map (go_go_gadget_inverse modulus) vec



go_go_gadget_pad :: (Integral a, Integral b) => b -> a -> [a]
go_go_gadget_pad modulus x =
  let xs = go_go_gadget_elem x
  in
   let pad = (floor $ logBase 2 $ fromIntegral modulus) - (length xs)
   in
    ([0 | i <- [0..pad]]++xs)



go_go_gadget_compose :: (Integral a, Integral b) => a -> b -> [[a]] -> [[[a]]] -> [[a]]
go_go_gadget_compose modulus dim row mat = map (polydot modulus dim row) mat



polydot :: (Integral a, Integral b) => a -> b -> [[a]] -> [[a]] -> [a]
polydot modulus dim v1 v2 = reduce modulus dim $ foldl (+) [0 | i <- [0..((length v1)-1)]] $ zipWith (convolve modulus dim) v1 v2



dot :: (Integral a, Integral b) => a -> b -> [a] -> [[a]] -> [[a]]
dot modulus dim vec vecs = map (reduce modulus dim) ( map (convolve modulus dim vec) vecs)

