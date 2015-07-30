import System.Random

data Tree a = Leaf a | Node (Tree a) (Tree a)
data Binary = One | Zero

randomTree :: (RandomGen g) => g -> [Binary] -> (Tree Binary, g)
randomTree gen [] = error "No value with which to make tree"
randomTree gen [x] = (Leaf x, gen)
randomTree gen xs =
  let (leftLen, g') = randomR (1, length xs) gen
  in
     let (leftTree, g'') = randomTree g' (take leftLen xs)
     in
      let (rightTree, g''') = randomTree g'' (drop leftLen xs)
      in (Node leftTree rightTree, g''')
