
module Gadg where

import Zn
import Vec
import Poly



gInv' :: (Integral a, Integral c, Integral s) => 
        Poly c (Zn a) -> [Poly c (Zn a)]
gInv' (Poly c1 ((Zn m1 x):xs))
  | (filter ( <= (Zn m1 1)) xs) == xs = [Poly c1 xs]
  | otherwise = (Poly c1 $ map rem2 xs):(gInv' $ Poly c1 $ map quot2 xs)
                


