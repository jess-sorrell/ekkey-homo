
import Data.Reflection
import Data.Proxy
import Text.Printf
import System.Random
import Data.List


import Zn
import RandomTree
import Gadget
import Poly


calcAT :: (Integral b, Integral a, Reifies s a) => Tree Binary -> b -> a -> [Zn s a] -> [Zn s a] -> [Zn s a]
calcAT (Leaf Zero) _ _ a0 a1 = a0
calcAT (Leaf One) _ _ a0 a1 = a1
calcAt (Node l r) dim mod a0 a1 =
  let leftAT = calcAt l dim mod a0 a1
  in
   let rightAT = calcAt r dim mod a0 a1
   in go_go_gadget_compose dim leftAT rightAT



genRandModulus :: (Integral a, Random a, RandomGen g) => a -> g -> (a, g)
genRandModulus dim gen =
  let primes = primeSieve (dim*dim) (2*dim*dim)
  in
   let (index, gen') = randomR (0, length primes) gen
   in
    (primes!!index, gen')



primeSieve :: (Integral a) => a -> a -> [a]
primeSieve lower upper =
  let factors = [2..(floor $ sqrt $ fromIntegral upper)]
  in
   let range = [lower..upper]
   in
    foldl (\candPrimes divr -> filter (\x -> x `mod` divr /= 0) candPrimes) range factors


{--
main :: IO ()
main = do putStrLn "Seed please! "
          seed <- reads getLine
          putStrLn "And what dimensionality for your lattice today? "
          dimension <- reads getLine
          let g = mkStdGen
              modulus = --}
