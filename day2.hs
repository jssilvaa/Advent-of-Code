import Data.Functor ( (<&>))
import Control.Arrow ( (>>>) )

parseInput :: FilePath -> IO [String]
parseInput filename = readFile filename <&>
    (lines >>> (map (\(a:_:b:_) -> a : b : [])))

part :: Int -> [String] -> Int 
part i = if i == 1 then sum . map score else sum . map score' 
    
score :: String -> Int 
score "AX" = 4 
score "AY" = 8
score "AZ" = 3
score "BX" = 1
score "BY" = 5
score "BZ" = 9
score "CX" = 7
score "CY" = 2
score "CZ" = 6

score' :: String -> Int 
score' "AX" = 3
score' "AY" = 4
score' "AZ" = 8
score' "BX" = 1
score' "BY" = 5
score' "BZ" = 9
score' "CX" = 2
score' "CY" = 6
score' "CZ" = 7

main :: IO()
main = parseInput "input.txt" >>= (\x -> 
       putStrLn ("Part 1: " ++ (show (part 1 x)))
    >> putStrLn ("Part 2: " ++ (show (part 2 x))))