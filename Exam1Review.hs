-- Joel Lubinitsky

import Char

foo :: [a] -> Int
foo (x:xs) = 1
foo (x:_:xs) = 2
foo _ = 0

convert :: (Float, Float) -> Float
convert (foot, inch) = ((12 * foot) + inch) * 25.4

countdown :: Int -> [Int]
countdown n
	| n < 0 = [n]
	| n == 0 = [0]
	| n > 0 = n : countdown (n - 1)
	
insert :: Int -> [Int] -> [Int]
insert n (x:y:xs)
	| n > x && n < y = (x:n:y:xs)
	| otherwise = (x:insert n (y:xs))
	
smash :: String -> String
smash cs = [toLower x | x <- cs, isAlpha x]