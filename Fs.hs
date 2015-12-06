import System.Random
import Data.List

import qualified Data.ByteString as B
import Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8

import Zn
import RandomTree
import Gadget
import Poly



calculateAT :: (Integral b, Integral a) =>
          Tree Binary -> a -> b -> [[a]] -> [[a]] -> [[a]]
calculateAT (Leaf Zero) _ _ a0 _ = a0
calculateAT (Leaf One) _ _ _ a1 = a1
calculateAT (Node l r) m dim a0 a1 =
  let leftAT = calculateAT l m dim a0 a1
      rightAT = go_go_gadget_invector m $ calculateAT r m dim a0 a1
  in go_go_gadget_compose m dim leftAT rightAT



randModulus :: (Integral a, Random a, RandomGen g) => a -> g -> (a, g)
randModulus dim gen =
  let (m, g') = randomR (dim*dim, 2*dim*dim) gen
      in (m, g')



modularRounding :: (Integral a) => a -> a -> [[a]] -> [[a]]
modularRounding m target vec = map (\xs -> (map round (map (\x -> (toRational(target)/toRational(m))*(toRational x)) xs))) vec



-- Given a dimension, return reasonable values for source m,
-- target m, and random vectors a0 and a1
parameters :: (Integral a, Random a, RandomGen g) => a -> g -> ((a, a, [[a]], [[a]]), g)
parameters dim g =
  case isPowerOfTwo dim of
    True -> let (m, g') = randModulus dim g
                (target, g'') = getRandom m g' 
                l = ceiling $ logBase 2 $ fromIntegral m
                (a0, g''') = randomVectors m dim l g''
                (a1, g'''') = randomVectors m dim l g'''
            in ((m, target, a0, a1), g'''')
     


-- Calculate Fs(x) for string msg, dimension dim, secret key s,
-- and a random seed 
functionFs :: (Integral a, Random a) => String -> a -> [a] -> Int -> [[a]]
functionFs msg dim s seed =
  case (genericLength s == dim) of
    True ->
      let g = mkStdGen seed
          ((m, target, a0, a1), g') = parameters dim g
          (tree, g'') = toTree (B8.pack msg) g'
          at = calculateAT tree m dim a0 a1
          sAT = dot m dim s at
      in modularRounding m target sAT



main :: IO ()
main = do putStrLn "Seed please! "
          seed <- getLine
          putStrLn "And what dimensionality today? "
          dimension <- getLine
          putStrLn "So what're we randomizing? "
          msg <- getLine
          putStrLn "Excellent choice! "
          let dim = toInteger $ read dimension
              g = mkStdGen $ read seed
              ((m, target, a0, a1), g') = parameters dim g
              (tree, g'') = toTree (B8.pack msg) g'
              (key, g''') = getRandoms m dim g''
              (key1, g'''') = getRandoms m dim g'''
              key2 = reduce m dim (key - key1)
              output = functionFs msg dim key $ read seed
              out1 = functionFs msg dim key1 $ read seed
              out2 = functionFs msg dim key2 $ read seed

            in do putStrLn "Using base m "
                  putStrLn (show m)
                  putStrLn "Using target m "
                  putStrLn (show target)
                  putStrLn "F(x) with full key "
                  putStrLn $ show key
                  putStrLn "Yields output: "
                  putStrLn $ show output
                  putStrLn "F(x) with key: "
                  putStrLn $ show key1
                  putStrLn "Yields output: "
                  putStrLn $ show out1
                  putStrLn "F(x) with key: "
                  putStrLn $ show key2
                  putStrLn "Yields output: "
                  putStrLn $ show out2



                                          
       
                          
--print  (calcAT tree m dim a0 a1 )
