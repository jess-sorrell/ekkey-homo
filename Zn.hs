{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

import Data.Reflection
import Data.Proxy
import Text.Printf

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


znToIntegral :: Integral a => Zn a s -> a
znToIntegral (Zn x) = fromIntegral x

-- Convince the compiler that the phantom type in the proxy
-- is the same as the one in the Zn
likeProxy :: Proxy s -> Zn a s -> Zn a s
likeProxy _ = id

withZn :: Integral a => a -> (forall s. Reifies s a => Zn a s) -> a
withZn p z = reify p $ \proxy -> znToIntegral . likeProxy proxy $ z

main :: IO ()
main = print $ withZn (7::Int) (Zn 3 + Zn 5)
