{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Zn where

import Data.Reflection
import Data.Proxy
import Text.Printf
import System.Random
import Data.List


data Zn a = Zn a a deriving (Show, Eq)


instance (Integral a) => Num (Zn a) where
  Zn m1 x + Zn m2 y
            | m1 == m2 = Zn m1 $ (x + y) `mod` m1
            | otherwise = error "Incompatible Moduli"
  Zn m1 x * Zn m2 y
            | m1 == m2 = Zn m1 $ (x * y) `mod` m1
            | otherwise = error "Incompatible Moduli"
  abs (Zn m1 x) = Zn m1 (x `mod` m1)
  signum _ = error "Sign is meaningless"
  fromInteger x = error "No modulus provided" 
  negate (Zn m1 x) = Zn m1 $ (negate x) `mod` m1

-- data Peano = Zero | Succ Peano deriving (Eq, Show)

-- class Church n where
--   toValue :: (Integral a) => n -> a
--   fromValue :: (Integral a) => a -> n
  
-- instance Church Peano where
--   toValue Zero = 0
--   toValue (Succ a) = 1 + toValue a
--   fromValue 0 = Zero
--   fromValue x = Succ (fromValue (x-1))
  
-- newtype Zn n a = Zn a deriving (Eq)

-- instance (Show a) => Show (Zn n a) where
--   show (Zn a) = show a

-- modM :: (Integral a) => Peano -> a -> Zn n a
-- modM n x = Zn (x `mod` ( toValue n ))







-- instance (Integral a) => Num (Zn a a) where
--   Zn x + Zn y = 

-- instance (Integral a, Show a, Reifies s a) => Show (Zn s a) where
--        show x = 
--            let 
--                p = reflect (Proxy::Proxy s)           
--            in
--                printf "(%s mod %s)" (show (mod (getZ x) p)) (show p)


-- instance (Integral a, Reifies s a) => Num (Zn s a) where
--       Zn x + Zn y = Zn (mod (x + y) (reflect (Proxy::Proxy s)))
--       Zn x - Zn y = Zn (mod (x - y) (reflect (Proxy::Proxy s)))
--       Zn x * Zn y = Zn (mod (x * y) (reflect (Proxy::Proxy s)))
--       negate (Zn x) = Zn ((reflect (Proxy::Proxy s)) - x)
--       abs = id
--       signum x@(Zn 0) = x
--       signum _ = Zn 1
--       fromInteger x = Zn (mod (fromInteger x) p)
--             where p = reflect (Proxy :: Proxy s)



-- instance (Integral a, Random a, Reifies s a) => Random (Zn s a) where
--     random = let high = reflect (Proxy::Proxy s) - 1
--              in \g -> let (x,g') = randomR (0,high) g
--                       in (Zn x, g')
--     randomR _ = error "randomR non-sensical for Zq types"



-- znToIntegral :: Integral a => Zn s a -> a
-- znToIntegral (Zn x) = fromIntegral x



-- -- | Convince the compiler that the phantom type in the proxy
-- -- | is the same as the one in the Zn
-- likeProxy :: Proxy s -> Zn s a -> Zn s a
-- likeProxy _ = id



-- withZn :: Integral a => a -> (forall s. Reifies s a => Zn s a) -> a
-- withZn modulus z = reify modulus $ \proxy -> znToIntegral.likeProxy proxy $ z



-- -- | returns a list of random integers
-- getRandoms :: (Integral a, Integral b, Random a, RandomGen g) => a -> b -> g -> ([a], g)
-- getRandoms modulus 0 gen = ([], gen)
-- getRandoms modulus num gen =
--   let (rand, gen') = getRandom modulus gen
--   in
--    let (rands, gen'') = getRandoms modulus (num-1) gen'
--    in (rand:rands, gen'')



-- randomVectors :: (Integral a, Integral b, Random a, RandomGen g) => a -> b -> a -> g -> ([[a]], g)
-- randomVectors modulus dim 0 gen = ([], gen)
-- randomVectors modulus dim num gen =
--   let (vect, gen') = getRandoms modulus dim gen
--   in
--    let (vects, gen'') = randomVectors modulus dim (num-1) gen'
--    in (vect:vects, gen'')

  

   
-- -- | Returns an integer mod n between -floor(n/2) and ceil(n/2)
-- centeredLift :: (Integral a) => a -> a -> a
-- centeredLift modulus x
--   | (x `mod` modulus) <= modulus `div` 2 = (x `mod` modulus)
--   | otherwise = (x `mod` modulus) - modulus



-- getRandom :: (Integral a, Random a, RandomGen g) =>
--              a -> g -> (a, g)
-- getRandom modulus gen = randomR (0, modulus - 1) gen
                       


-- getCenteredRandoms :: (Integral a, Integral b, Random a, RandomGen g) =>
--                       a -> b -> g -> ([a], g)
-- getCenteredRandoms modulus 0 gen = ([], gen)
-- getCenteredRandoms modulus num gen =
--   let (rand, gen') = getRandom modulus gen
--   in
--    let (rands, gen'') = getCenteredRandoms modulus (num-1) gen'
--    in ( (centeredLift modulus rand):rands, gen'')



-- showZns :: (Integral a, Reifies s a) => a -> ([Zn s a]) -> [a]
-- showZns modulus [] = []
-- showZns modulus ((Zn x):xs) = (withZn modulus (Zn x )):(showZns modulus xs)

-- --main :: IO ()
-- --main = print $ withZn (7::Int) (Zn 3 + Zn 5)

