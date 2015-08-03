
import Data.Reflection
import Data.Proxy
import Text.Printf
import System.Random
import Data.List

import qualified Data.ByteString as B
import Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8

import Zn
import RandomTree
import Gadget
import Poly



calcAT :: (Integral b, Integral a) =>
          Tree Binary -> a -> b -> [a] -> [a] -> [a]
calcAT (Leaf Zero) _ _ a0 a1 = a0
calcAT (Leaf One) _ _ a0 a1 = a1
calcAT (Node l r) modulus dim a0 a1 =
  let leftAT = calcAT l modulus dim a0 a1
  in
   let rightAT = calcAT r modulus dim a0 a1
   in go_go_gadget_innerproduct modulus dim leftAT rightAT



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




main :: IO ()
main = do putStrLn "Seed please! "
          seed <- getLine
          putStrLn "And what dimensionality for your lattice today? "
          dimension <- getLine
          putStrLn "And what about a key value? "
          keyVal <- getLine
          putStrLn "So what're we encrypting? "
          msg <- getLine
          let dim = toInteger $ read dimension
           in
           let key = toInteger $ read keyVal
           in
           let gen = mkStdGen $ read seed
           in let (modulus, g') = genRandModulus (dim::Integer) gen
              in let (a0, g'') = getRandoms (modulus::Integer) (dim::Integer) g'
                 in let (a1, g''') = getRandoms modulus dim g''
                    in let (tree, g'''') = toTree (B8.pack msg) g'''
                       in print $ go_go_gadget_vector modulus a1

                          
--print  (calcAT tree modulus dim a0 a1 )
