-- Advent of Code :: Day 6 --
import Control.Arrow ( (>>>) )
import Data.List ( nub )

parseInput :: FilePath -> IO String
parseInput fs = readFile fs 

part :: Int -> String -> String
part n = foldl (\str c -> if length str == n then str 
                                           else if c `elem` str then (delete c str) ++ [c]
                                                                else str ++ [c]) "" 
    where delete :: Char -> String -> String 
          delete _ [] = []
          delete c (h:t) | h == c = t 
                         | otherwise = delete c t 

findIndex :: Int -> Int -> String -> String -> Int 
findIndex n acc str sub = let p = take n str 
                          in if p == sub then acc + n - 1
                                       else findIndex n (acc+1) (drop 1 str) sub 


main = do input <- parseInput "input.txt"
          putStrLn ("Parte 1 : " ++ (show (findIndex 4 1 input (part 4 input))))
          putStrLn ("Parte 2 : " ++ (show (findIndex 14 1 input (part 14 input))))
