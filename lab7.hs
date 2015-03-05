-- Joel Lubinitsky
-- CIS 252 - Lab 7
-- 03/06/15

import Data.Char

sqRoots :: [Float] -> [Float]
sqRoots lst = map sqrt lst

getLengths :: [String] -> [(String, Int)]
getLengths strs = map (\x -> (x, length x)) strs

negatives :: [Integer] -> [Integer]
negatives nums = filter (\x -> x < 0) nums

vowels :: [(Char, Int)] -> [(Char, Int)]
vowels pairs = filter (\x -> isVowel (fst x)) pairs
	where
		isVowel c = elem (toLower c) "aeiou"

mystery :: Int -> [Int] -> [Int]
mystery w zs = [ z + 25 | z <- zs, z <= w]

mystery2 :: Int -> [Int] -> [Int]
mystery2 w zs = map (\x -> x + 25) (filter (\y -> y <= w) zs)