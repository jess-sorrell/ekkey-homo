{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, Rank2Types #-}

import Data.Proxy
import Data.Reflection as Ref
import Text.Printf

data Zn n a = Zn !a !a

instance (Integral a) => Eq (Zn n a) where
  Zn n x == Zn _ y = mod (x - y) n == 0

instance (Integral a, Show a) => Show (Zn n a) where
  show (Zn n x) = printf "(%s mod %s)" (show (mod x n)) (show n)

instance (Integral a, Reifies n a) => Num (Zn n a) where
--  Zn n x + Zn _ y = Zn n (mod (x + y) n)
  Zn n x + Zn _ y = Zn (fromIntegral n) $ withModulus' n (fromIntegral x + fromIntegral y)
  Zn n x - Zn _ y = Zn n (mod (x - y) n)
  Zn n x * Zn _ y = Zn n (mod (x * y) n)
  negate (Zn n x) = Zn n (n - x)
  abs = id
  signum x@(Zn _ 0) = x
  signum (Zn n _) = Zn n 1
  fromInteger x = Zn n (mod (fromInteger x) n)
    where n = reflect (Proxy :: Proxy n)

znToInteger :: Integral a => Zn n a -> a
znToInteger (Zn n x) = fromIntegral x


withModulus :: forall a b. (Integral a) => a ->
               (forall n. (Reifies n a) => (a -> Zn n a) ->
                (Zn n a -> a) -> b) -> b
withModulus n k = reify n $ \(_ :: Proxy n) ->
  k (\x -> Zn n (mod x n) :: Zn n a) (\(Zn _ x) -> x)


withModulus' :: forall a. (Integral a) => a -> (forall n. (Reifies n a) => Zn n a) -> a
withModulus' n mx = reify n $ \(_ :: Proxy n) ->
            case mx :: Zn n a of
              Zn _ x -> x
