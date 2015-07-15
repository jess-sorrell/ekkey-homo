{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable,
             FlexibleContexts, FlexibleInstances,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
              PolyKinds, 
             RoleAnnotations, ScopedTypeVariables, 
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}
import Data.Proxy
import Reflects
import Text.Printf
import Unsafe.Coerce as Yolo
import GHC.TypeLits  as TL
import Data.Typeable
newtype Zn n i = Zn i deriving (Eq, Show)

type ReflectsTI n i = (Reflects n i, Integral i)

type role Zn representational representational
instance (ReflectsTI n i) => Num (Zn n i) where

  (+) = let nval = value (Proxy::Proxy n)
        in \ (Zn x) (Zn y) ->
        let z = x + y
        in Zn ( z `mod` nval)

  (-) = let nval = value (Proxy::Proxy n)
        in \ (Zn x) (Zn y) ->
        let z = x - y
        in Zn ( z `mod` nval)

  (*) = let nval = value (Proxy:: Proxy n)
        in \ (Zn x) (Zn y) ->
        let z = x *y
        in Zn (z `mod` nval) 

  negate = let nval = value (Proxy::Proxy n)
           in \ (Zn x) ->
           let z = (nval - x) 
           in Zn ( z `mod` nval)

  abs = id

  signum x @(Zn 0) = x
  signum (Zn _) = Zn 1
  
  fromInteger x = Zn (mod (fromInteger x) n)
    where n = value (Proxy :: Proxy n)
          
znToIntegral :: (ReflectsTI n i) => Zn n i -> i
znToIntegral (Zn i) = i



{--reduce' :: forall i n. (ReflectsTI n i) => i -> Zn n i 
reduce' = Yolo.unsafeCoerce . (`mod` proxy value (Proxy::Proxy n))                  --       value = return $ fromIntegral $ natVal (Proxy::Proxy a)--}


getModulus :: forall i n. (Integral i, Reflects n i) => Zn n i -> Zn n i
getModulus (Zn x) = Zn $ value (Proxy::Proxy n)













    
