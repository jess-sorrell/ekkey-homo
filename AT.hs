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
          Tree Binary -> a -> b -> [[a]] -> [[a]] -> [[a]]
calcAT (Leaf Zero) _ _ a0 _ = a0
calcAT (Leaf One) _ _ _ a1 = a1
calcAT (Node l r) modulus dim a0 a1 =
  let leftAT = calcAT l modulus dim a0 a1
  in
   let rightAT = go_go_gadget_invector modulus $ calcAT r modulus dim a0 a1
   in go_go_gadget_compose modulus dim leftAT rightAT



genRandModulus :: (Integral a, Random a, RandomGen g) => a -> g -> (a, g)
genRandModulus dim gen =
  let primes = primeSieve (dim*dim) (2*dim*dim)
  in
   let (index, gen') = randomR (0, length primes) gen
   in
    (primes!!index, gen')



primeSieve :: (Integral a) => a -> a -> [a]
primeSieve lower upper =
  let factors = [2..(ceiling $ sqrt $ fromIntegral upper)]
  in
   let range = [lower..upper]
   in
    foldl (\candPrimes divr -> filter (\x -> x `mod` divr /= 0) candPrimes) range factors




ringSwitch :: (Integral a) => a -> a -> [[a]] -> [[a]]
ringSwitch modulus target vec = map (\xs -> (map round (map (\x -> (toRational(target)/toRational(modulus))*(toRational x)) xs))) vec



keyedRingSwitch :: (Integral a, Integral b) => [a] -> a -> b -> a -> [[a]] -> [[a]]
keyedRingSwitch s modulus dim target vec =
  let sAT = dot modulus dim s vec
  in
   ringSwitch modulus target sAT



isPowerOfTwo :: (Integral a) => a -> Bool
isPowerOfTwo 1 = True
isPowerOfTwo n
  | n `mod` 2 == 0 = isPowerOfTwo $ n `div` 2
  | otherwise = False
                


-- Given a dimension, return reasonable values for source modulus,
-- target modulus, and random vectors a0 and a1
parameters :: (Integral a, Random a, RandomGen g) => a -> g -> ((a, a, [[a]], [[a]]), g)
parameters dim gen =
  case  isPowerOfTwo dim of
    True -> let (modulus, g') = genRandModulus dim gen
                (target, g'') = getRandom modulus g' 
                l = ceiling $ logBase 2 $ fromIntegral modulus
                (a0, g''') = getRandomVectors modulus dim l g''
                (a1, g'''') = getRandomVectors modulus dim l g'''
            in ((modulus, target, a0, a1), g'''')
     

main :: IO ()
main = do putStrLn "Seed please! "
          seed <- getLine
          putStrLn "And what dimensionality for your lattice today? "
          dimension <- getLine
          putStrLn "So what're we randomizing? "
          msg <- getLine
          putStrLn "Excellent choice! "
          let dim = toInteger $ read dimension
              gen = mkStdGen $ read seed
              ((modulus, target, a0, a1), g') = parameters dim gen
              (tree, g'') = toTree (B8.pack msg) g'
              at = calcAT tree modulus dim a0 a1
              (key, g''') = getRandoms modulus dim g''
              (key1, g'''') = getRandoms modulus dim g'''
              key2 = reduce modulus dim (key - key1)
              output = keyedRingSwitch key modulus dim target at
              out1 = keyedRingSwitch key1 modulus dim target at
              out2 = keyedRingSwitch key2 modulus dim target at
            in do putStrLn "Using base modulus "
                  putStrLn (show modulus)
                  putStrLn "Using target modulus "
                  putStrLn (show target)
                  putStrLn "Hashing with full key "
                  putStrLn $ show key
                  putStrLn "Yields output: "
                  putStrLn $ show output
                  putStrLn "Hashing with key: "
                  putStrLn $ show key1
                  putStrLn "Yields output: "
                  putStrLn $ show out1
                  putStrLn "Hashing with key: "
                  putStrLn $ show key2
                  putStrLn "Yields output: "
                  putStrLn $ show out2



                                          
       
                          
--print  (calcAT tree modulus dim a0 a1 )
