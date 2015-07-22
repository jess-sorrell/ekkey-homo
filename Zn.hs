{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Zn where

import Data.Reflection
import Data.Proxy
import Text.Printf
import Crypto.Random.DRBG
import System.Random
import Data.List

newtype Zn a s = Zn { getZ :: a } 

instance (Integral a, Show a, Reifies s a) => Show (Zn a s) where
       show x = 
           let 
               p = reflect (Proxy::Proxy s)           
           in
               printf "(%s mod %s)" (show (mod (getZ x) p)) (show p)


instance (Integral a, Reifies s a) => Num (Zn a s) where
      Zn x + Zn y = Zn (mod (x + y) (reflect (Proxy::Proxy s)))
      Zn x - Zn y = Zn (mod (x - y) (reflect (Proxy::Proxy s)))
      Zn x * Zn y = Zn (mod (x * y) (reflect (Proxy::Proxy s)))
      negate (Zn x) = Zn ((reflect (Proxy::Proxy s)) - x)
      abs = id
      signum x@(Zn 0) = x
      signum _ = Zn 1
      fromInteger x = Zn (mod (fromInteger x) p)
            where p = reflect (Proxy :: Proxy s)


instance (Integral a, Random a, Reifies s a) => Random (Zn a s) where
    random = let high = reflect (Proxy::Proxy s) - 1
             in \g -> let (x,g') = randomR (0,high) g
                      in (Zn x, g')
    randomR _ = error "randomR non-sensical for Zq types"


znToIntegral :: Integral a => Zn a s -> a
znToIntegral (Zn x) = fromIntegral x


-- | Convince the compiler that the phantom type in the proxy
-- | is the same as the one in the Zn
likeProxy :: Proxy s -> Zn a s -> Zn a s
likeProxy _ = id


withZn :: Integral a => a -> (forall s. Reifies s a => Zn a s) -> a
withZn p z = reify p $ \proxy -> znToIntegral . likeProxy proxy $ z


-- | Given an Integral modulus and a number of desired integers, returns a
-- | list of random integers
getRandoms :: (Integral a, RandomGen g) => a -> Integer -> g -> [Integer]
getRandoms modulus x g =
  genericTake x $map (\n -> mod n (toInteger modulus)) $ randoms g::[Integer] 


-- | Returns an integer mod n between -floor(n/2) and ceil(n/2)
centeredLift :: (Integral a) => a -> a -> a
centeredLift arg modulus
  | (arg `mod` modulus) <= modulus `div` 2 = (arg `mod` modulus)
  | otherwise = (arg `mod` modulus) - modulus


getCenteredRandom :: (Integral a) => a -> Integer -> IO()
getCenteredRandom = undefined

--main :: IO ()
--main = print $ withZn (7::Int) (Zn 3 + Zn 5)
