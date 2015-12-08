
module Gadg where

import Data.List

import Zn
import Vec
import Poly



gInvPoly' :: (Integral a, Integral c) => 
        Poly c (Zn a) -> [Poly c (Zn a)]
gInvPoly' (Poly c1 zs@((Zn m1 x):xs))
  | (filter ( <= (Zn m1 1)) zs) == zs = [Poly c1 zs]
  | otherwise = (Poly c1 $ map rem2 zs):(gInvPoly' $ Poly c1 $ map quot2 zs)


-- Binary expansion of coefficients of given polynomial
gInvPoly ::  (Integral a, Integral c, Integral s) => 
         Poly c (Zn a) -> Vec s (Poly c (Zn a))
gInvPoly p = fromZnPolyList $ gInvPoly' p



gInv :: (Integral a, Integral c, Integral s) =>
           Vec s (Poly c (Zn a)) -> Vec s (Vec s (Poly c (Zn a)))
gInv (Vec dim ps) =
  let vs = map gInvPoly ps
      d = genericLength vs
  in
   Vec d vs



gCompose :: (Integral a, Integral c, Integral s) =>
            Vec s (Poly c (Zn a)) -> Vec s (Vec s (Poly c (Zn a)))
            -> Vec s (Poly c (Zn a))
gCompose v@(Vec d1 ps) (Vec d2 vs) = Vec d1 $ map ( dot v ) vs    


-- let x = (Poly 4 [Zn 7 6, Zn 7 3])
-- let y = (Poly 4 [Zn 7 2, Zn 7 1])
-- let z = (Poly 4 [Zn 7 1, Zn 7 6])

-- --let v = Vec 3 [x, y, z]

-- let vs = Vec 3 [v, v, v]

-- gCompose v vs
-- dot v v


 
