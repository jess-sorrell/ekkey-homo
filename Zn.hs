 {-# LANGUAGE FlexibleContexts, ConstraintKinds, PolyKinds, ScopedTypeVariables, RankNTypes, DataKinds #-}

import Data.Proxy
import Data.Reflection
import Text.Printf
import qualified Unsafe.Coerce as Yolo
import qualified GHC.TypeLits  as TL
import qualified Algebra.Additive as Additive(C)

newtype Zn n i = Zn i deriving (Eq)

type ReifiesTI n i = (Reifies n i, Integral i)

instance (ReifiesTI n i) => Show (Zn n i) where
  show z = show $ znToInteger z

instance (ReifiesTI n i) => Num (Zn n i) where
  
  (+) = let nval = reflect (Proxy::Proxy n)
        in \ (Zn x) (Zn y) ->
        let z = x + y
        in Zn ( z `mod` nval)

  (-) = let nval = reflect (Proxy::Proxy n)
        in \ (Zn x) (Zn y) ->
        let z = x - y
        in Zn ( z `mod` nval)

  (*) = let nval = reflect (Proxy:: Proxy n)
        in \ (Zn x) (Zn y) ->
        let z = x *y
        in Zn (z `mod` nval) 

  negate = let nval = reflect (Proxy::Proxy n)
           in \ (Zn x) ->
           let z = (nval - x) 
           in Zn ( z `mod` nval)

  abs = id

  signum x @(Zn 0) = x
  signum (Zn _) = Zn 1
  
  fromInteger x = Zn (mod (fromInteger x) n)
    where n = reflect (Proxy :: Proxy n)
          
znToInteger :: (ReifiesTI n i) => Zn n i -> Integer
znToInteger (Zn i) = fromIntegral i



reduce' :: forall i n. (ReifiesTI n i) => i -> Zn n i 
reduce' = Yolo.unsafeCoerce . (`mod` reflect (Proxy::Proxy n))                  --       value = return $ fromIntegral $ natVal (Proxy::Proxy a)


getModulus :: forall i n. (Integral i, Reifies n i) => Zn n i -> Zn n i
getModulus (Zn x) = Zn $ reflect (Proxy::Proxy n)













    
