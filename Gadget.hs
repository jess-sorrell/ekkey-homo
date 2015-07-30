{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

import Data.Reflection
import Data.Proxy
import Text.Printf
import System.Random
import Zn
import Poly


get_get_gadget :: (Integral a) => a -> [Zn s a] 
get_get_gadget modulus =
  let l = ceiling $ logBase 2 $ fromIntegral modulus
  in [Zn (2^i) | i <- [0..l]]

go_go_gadget_expand :: (Integral a, Reifies s a) => Zn s a -> [Zn s a]
go_go_gadget_expand (Zn 0) = [Zn 0]
go_go_gadget_expand (Zn 1) = [Zn 1]
go_go_gadget_expand (Zn x) = (Zn ( x `mod` 2)):(go_go_gadget_zn $ Zn (x `quot` 2))

go_go_gadget_vector :: (Integral a, Reifies s a) => [Zn s a] -> [Zn s a]
go_go_gadget_vector [] = []
go_go_gadget_vector (x:xs) = (go_go_gadget_zn x)++(go_go_gadget_vector xs)


go_go_gadget_compose :: (Integral a, Reifies s a) => [Zn s a] -> [Zn s a] -> [Zn s a]
go_go_gadget_compose as expanded
  | len expanded == len as = 
  | len expanded `mod` len as == 0 =
    let dim = len as
    in take dim expanded 
    | otherwise = error "What do you want me to do with this?"
