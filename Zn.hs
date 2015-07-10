{-# LANGUAGE Rank2Types, UndecidableInstances, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, KindSignatures, PolyKinds, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}


import qualified Algebra.Additive   as Additive (C(..))
import qualified Algebra.Module     as Module 
import qualified Algebra.Ring       as Ring (C(..)) 
import           Data.Traversable   as DT
import qualified Number.ResidueClass as RC
import Data.Natural
import Data.Reflection (Reifies, reflect, reify)
import Data.Proxy

class ReifiesInt n where
  reflect :: Proxy n -> Int

newtype Zq q = Zq Integer deriving (Eq, Ord, Show)

data Modulus s = M {getModulus :: Integer}
type Modular s = Reifies s Integer

normalize :: forall s . Modular s => Integer -> Modulus s
normalize n = M (mod n modulus) where
  modulus = Data.Reflection.reflect (Proxy :: Proxy s)

instance Modular s => Num (Modulus s) where
  M a + M b = normalize (a + b)
  M a * M b = normalize (a * b)

withModulus :: Integer -> (forall s . Modular s =>  Modulus s) -> Integer
withModulus m v = Data.Reflection.reify m (getModulus . asProxyOf v)
                  where asProxyOf :: f s -> Proxy s -> f s
                        asProxyOf = const
{--instance (ReifiesInt q) => Num (Zq q) where
  Zq x + Zq y = Zq (x + y `mod` reflect ([]:[q])) --}
{--
instance Pos q => Show (Zq q) where
  show (Zq q) = show q

instance Natural q => Num (Zq q) where
  Zq x + Zq y = Zq $ (x+y) `mod`  (undefined::n)
  Zq x * Zq y = Zq $ (x*y) `mod`  (undefined::n) 
  negate (Zq x) = Zq $ (negate x) `mod`  (undefined::n)
  fromInteger x = Zq $ fromInteger $ x `mod`  (undefined::n)--}
