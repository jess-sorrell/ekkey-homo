import System.Random
import Data.List

import qualified Data.ByteString as B
import Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8

import Zn
import RandomTree
import Gadget




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
ringSwitch p q vec = map (\xs -> (map round (map (\x -> (toRational(q)/toRational(p))*(toRational x)) xs))) vec


main :: IO ()
main = do putStrLn "Seed please! "
          seed <- getLine
          putStrLn "And what dimensionality for your lattice today? "
          dimension <- getLine
          putStrLn "And what about a key value? "
          keyVal <- getLine
          putStrLn "So what're we randomizing? "
          msg <- getLine
          putStrLn "Excellent choice! "
          let dim = toInteger $ read dimension
            in let key = toInteger $ read keyVal
               in let gen = mkStdGen $ read seed
                  in let (p, g') = genRandModulus dim gen
                     in let (q, g'') = genRandModulus dim g'
                            modulus = minimum [p, q]
                            target = maximum [p, q]
                        in let l = ceiling $ logBase 2 $ fromIntegral modulus
                           in let (a0, g''') = getRandomVectors modulus dim l g'
                              in let (a1, g'''') = getRandomVectors modulus dim l g''
                                 in let (tree, g''''') = toTree (B8.pack msg) g'''
                                    in let at = calcAT tree modulus dim a0 a1
                                           output = ringSwitch modulus target at
                                           in print output
                                       
       
                          
--print  (calcAT tree modulus dim a0 a1 )
