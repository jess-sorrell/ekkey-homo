{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
module Vec where

import System.Random

import Poly
import Zn
import Data.List

data Vec s a = Vec s [a] deriving (Eq)


fromPolyList :: (Num a, Integral c, Integral s) =>
                [Poly c a] -> Vec s (Poly c a)
fromPolyList xs = Vec (genericLength xs) xs 


-- Helper function for gInv.
-- Having difficult constructing g^-1(a) to have the appropriate number
-- of dimensions, so ZnPolyList will pad out the list of polynomials
-- produced inside gInv
fromZnPolyList :: (Integral a, Integral c, Integral s) =>
                  [Poly c (Zn a)] -> Vec s (Poly c (Zn a))
fromZnPolyList [] = error "Can't infer dimension from empty list"
fromZnPolyList ((Poly c1 []):xs) = error "Can't infer dimension from 0 elmt"
fromZnPolyList ps@((Poly c1 ((Zn m z):zs)):xs)
  | (ceiling $ logBase 2 $ fromIntegral m) == genericLength ps =
    fromPolyList ps
  | (ceiling $ logBase 2 $ fromIntegral m) > genericLength ps =
    let pad = (ceiling $ logBase 2 $ fromIntegral m) - (genericLength ps)
    in fromPolyList $ ps++(genericReplicate pad (zeroZnPoly c1 m))
  | otherwise = error "Poly list dimension is greater than ceil (ln modulus)"
 


-- Vec is an instance of Show
instance (Show a) => Show (Vec s a) where
  show (Vec s1 xs) = "{ " ++ (tail . init) (show xs)  ++ " }"



-- Vec is an instance of Random
instance (Random a, Integral a, Integral c, Integral s) =>
         Random (s -> c -> a -> Vec s (Poly c (Zn a))) where
  random g =
    let (g', g'') = split g
    in ((\dim c1 m ->
         Vec dim $ map (\f -> f c1 m) (genericTake dim $ (randoms g'))), g'')
  randomR = error "Range is not meaningfully defined for Vec"



randomVec :: (Random a, Integral a, Integral c, Integral s, RandomGen g) =>
  g -> s -> c -> a -> (Vec s (Poly c (Zn a)), g) 
randomVec gen dim c1 m =
  let (pls, gen') =
        foldl (\(ps, g) _ ->
                let (p, g') = randomPoly g c1 m
                in ( (p:ps), g')) ([], gen) [1..dim]
  in (Vec dim pls, gen')
  


vecLength :: (Num a, Integral s) => Vec s a -> s
vecLength (Vec s xs) = genericLength xs


-- Ugh
dot :: (Integral a, Integral c, Integral s) =>
       Vec s (Poly c (Zn a)) -> Vec s (Poly c (Zn a)) -> (Poly c (Zn a))
dot (Vec s1 v1@((Poly c1 ((Zn m1 x):xs)):p1s)) (Vec s2 v2@((Poly c2 p2):p2s))
  = foldl (+) (zeroZnPoly c1 m1) $ zipWith (*) v1 v2


scalarMult :: (Integral a, Integral c, Integral s) =>
              Poly c (Zn a) -> Vec s (Poly c (Zn a)) ->
              Vec s (Poly c (Zn a))
scalarMult p (Vec s1 ps) = Vec s1 $ map (p * ) ps


vecSwitch :: (Integral a, Integral c, Integral s) =>
             a -> Vec s (Poly c (Zn a)) -> Vec s (Poly c (Zn a))
vecSwitch targ (Vec dim ps) = Vec dim $ map (polySwitch targ) ps

