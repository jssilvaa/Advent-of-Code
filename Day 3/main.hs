import Data.Functor ( (<&>))
import Control.Arrow ( (>>>) )
import Data.Char ( ord )
import Data.List ( intersect )

groupInto :: Int -> [a] -> [[a]]
groupInto _ [] = []
groupInto 0 _ = []
groupInto n xs = [a] ++ groupInto n b 
    where (a,b) = splitAt n xs 

assign :: Char -> Int 
assign c | ord 'a' <= ord c && ord c <= ord 'z' = ord c - ord 'a' + 1 
         | otherwise = ord c - ord 'A' + 27

parseInput :: FilePath -> IO [String]
parseInput filename = readFile filename <&> lines

part1 :: [String] -> Int 
part1 = map (\x -> splitAt (div (length x) 2) x) 
    >>> map (\(x,y) -> intersect x y) 
    >>> (map (assign . head) >>> sum)

part2 :: [String] -> Int
part2 = groupInto 3 
    >>> (map (\(x:y:z:_) -> head $ intersect x (intersect y z)))
    >>> map assign >>> sum 

main = parseInput "input.txt" >>= (\x -> 
       putStrLn ("Parte 1: " ++ show (part1 x))
    >> putStrLn ("Parte 2: " ++ show (part2 x)))
