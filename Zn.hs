{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Zn where

import Data.Reflection
import Data.Proxy
import Text.Printf
import System.Random
import Data.List

newtype Zn s a = Zn { getZ :: a } 

instance (Integral a, Show a, Reifies s a) => Show (Zn s a) where
       show x = 
           let 
               p = reflect (Proxy::Proxy s)           
           in
               printf "(%s mod %s)" (show (mod (getZ x) p)) (show p)


instance (Integral a, Reifies s a) => Num (Zn s a) where
      Zn x + Zn y = Zn (mod (x + y) (reflect (Proxy::Proxy s)))
      Zn x - Zn y = Zn (mod (x - y) (reflect (Proxy::Proxy s)))
      Zn x * Zn y = Zn (mod (x * y) (reflect (Proxy::Proxy s)))
      negate (Zn x) = Zn ((reflect (Proxy::Proxy s)) - x)
      abs = id
      signum x@(Zn 0) = x
      signum _ = Zn 1
      fromInteger x = Zn (mod (fromInteger x) p)
            where p = reflect (Proxy :: Proxy s)



instance (Integral a, Random a, Reifies s a) => Random (Zn s a) where
    random = let high = reflect (Proxy::Proxy s) - 1
             in \g -> let (x,g') = randomR (0,high) g
                      in (Zn x, g')
    randomR _ = error "randomR non-sensical for Zq types"



znToIntegral :: Integral a => Zn s a -> a
znToIntegral (Zn x) = fromIntegral x



-- | Convince the compiler that the phantom type in the proxy
-- | is the same as the one in the Zn
likeProxy :: Proxy s -> Zn s a -> Zn s a
likeProxy _ = id



withZn :: Integral a => a -> (forall s. Reifies s a => Zn s a) -> a
withZn modulus z = reify modulus $ \proxy -> znToIntegral.likeProxy proxy $ z



-- | returns a list of random integers
getRandoms :: (Integral a, Integral b, Random a, RandomGen g) => a -> b -> g -> ([a], g)
getRandoms modulus 0 gen = ([], gen)
getRandoms modulus num gen =
  let (rand, gen') = getRandom modulus gen
  in
   let (rands, gen'') = getRandoms modulus (num-1) gen'
   in (rand:rands, gen'')


-- | Returns an integer mod n between -floor(n/2) and ceil(n/2)
centeredLift :: (Integral a) => a -> a -> a
centeredLift modulus x
  | (x `mod` modulus) <= modulus `div` 2 = (x `mod` modulus)
  | otherwise = (x `mod` modulus) - modulus



getRandom :: (Integral a, Random a, RandomGen g) =>
             a -> g -> (a, g)
getRandom modulus gen = randomR (0, modulus - 1) gen
                       


getCenteredRandoms :: (Integral a, Integral b, Random a, RandomGen g) =>
                      a -> b -> g -> ([a], g)
getCenteredRandoms modulus 0 gen = ([], gen)
getCenteredRandoms modulus num gen =
  let (rand, gen') = getRandom modulus gen
  in
   let (rands, gen'') = getCenteredRandoms modulus (num-1) gen'
   in ( (centeredLift modulus rand):rands, gen'')



{--
getCenteredZns :: (Integral a, Random a, RandomGen g, Reifies s a) => a -> a -> g -> ([a], g)
getCenteredZns modulus num gen = getCenteredRandoms modulus num gen --}




showZns :: (Integral a, Reifies s a) => a -> ([Zn s a]) -> [a]
showZns modulus [] = []
showZns modulus ((Zn x):xs) = (withZn modulus (Zn x )):(showZns modulus xs)

--main :: IO ()
--main = print $ withZn (7::Int) (Zn 3 + Zn 5)

