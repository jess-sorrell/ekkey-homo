import System.Random
import Data.List

import qualified Data.ByteString as B
import Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8

import Zn
import RandomTree
import Gadg
import Vec
import Poly



calculateAT :: (Integral a, Integral c, Integral s) =>
               Tree Binary ->  Vec s (Poly c (Zn a)) ->
               Vec s (Poly c (Zn a)) -> Vec s (Poly c (Zn a))
calculateAT (Leaf Zero) a0 _ = a0
calculateAT (Leaf One)  _ a1 = a1
calculateAT (Node l r) a0 a1 =
  let leftAT = calculateAT l a0 a1
      rightAT = gInv $ calculateAT r a0 a1
  in gCompose leftAT rightAT



randModulus :: (Integral a, Random a, RandomGen g) =>
               g -> a -> (a, g)
randModulus g n =
  let n' = fromIntegral n
  in randomR (n'*n', 2*n'*n') g

                      
-- How to bound target modulus by other parameters?
randTarget :: (Integral a, Random a, RandomGen g) =>
              g -> a -> (a, g)
randTarget g mod = randomR ((round . sqrt . fromIntegral) mod, mod) g

  
-- Given a cyclotomic index, return reasonable values for source m,
-- target m, and random vectors a0 and a1
parameters :: (Integral a, Random a, RandomGen g, Integral s) =>
              a -> g -> ((a, a, Vec s (Poly a (Zn a)),
                          Vec s (Poly a (Zn a))), g)
parameters c1 g =
  case isPowerOfTwo c1 of
    True -> let n = c1 `div` 2
                (m, g') = randModulus g n
                (target, g'') = randTarget g' m 
                l = ceiling $ logBase 2 $ fromIntegral m 
                (a0, g''') = randomVec g'' l c1 m 
                (a1, g'''') = randomVec g''' l c1 m
            in (( m, target, a0, a1), g'''')
     


-- Calculate Fs(x) for string msg, dimension dim, secret key sk,
-- and a random seed 
functionFs :: (Integral a, Integral s, Random a) =>
              String -> a -> Poly a (Zn a) ->
              Int -> Vec s (Poly a (Zn a))
functionFs msg c1 sk seed =
  case (polyLength sk == (c1 `div` 2)) of
    True ->
      let g = mkStdGen seed
          ((m, target, a0, a1), g') = parameters c1 g
          (tree, g'') = toTree (B8.pack msg) g'
          at = calculateAT tree a0 a1
          sAT = scalarMult sk at
      in vecSwitch target sAT



main :: IO ()
main = do putStrLn "Seed (Int): "
          seed <- getLine
          putStrLn "Cyclotomic index (Integral, power of 2): "
          ind <- getLine
          putStrLn "String to randomize: "
          msg <- getLine
          let c = toInteger $ read ind
              g = mkStdGen $ read seed
              ((m, target, a0, a1), g') = parameters c g
              (tree, g'') = toTree (B8.pack msg) g'
              (key, g''') = randomPoly g'' c m 
              (key1, g'''') = randomPoly g''' c m
              key2 = reduce (key - key1)
              output = functionFs msg c key $ read seed
              out1 = functionFs msg c key1 $ read seed
              out2 = functionFs msg c key2 $ read seed

            in do putStrLn "*********************************** \n"
                  putStrLn $ "Using base modulus " ++ (show m)
                  putStrLn $ "Using target modulus" ++ (show target) ++"\n"
                  putStrLn "*********************************** \n"
                  putStrLn "F(x) with full key "
                  putStrLn $ show key
                  putStrLn "Yields output: "
                  putStrLn $ (show output) ++ "\n"
                  putStrLn "*********************************** \n"
                  putStrLn "F(x) with key: "
                  putStrLn $ show key1
                  putStrLn "Yields output: "
                  putStrLn $ (show out1) ++ "\n"
                  putStrLn "*********************************** \n"
                  putStrLn "F(x) with key: "
                  putStrLn $ show key2
                  putStrLn "Yields output: "
                  putStrLn $ (show out2)


                                          
       
                          
