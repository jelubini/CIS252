-- Joel Lubinitsky
-- CIS 252 - HW 7
-- 03/06/15

import Cards
import Data.List

countTrump :: Suit -> [Card] -> Int
countTrump trump cards = length (filter (\(Cd suit n) -> suit == trump) cards)

beatsNT :: Card -> Card -> Bool
beatsNT (Cd s1 n1) (Cd s2 n2) = not ((s1 == s2) && (n2 > n1))

beats :: Suit -> Card -> Card -> Bool
beats trump (Cd s1 n1) (Cd s2 n2)
	| (s1 /= trump) && (s2 == trump) = False
	| otherwise = not ((s1 == s2) && (n2 > n1))
	
hcPoints :: [Card] -> Int
hcPoints hand = (sum (map (\(Cd s n) -> n - 10) (filter (\(Cd s n) -> n > 10) hand))) + (4 * (length (filter (\(Cd s n) -> n == 1) hand)))

nOfAKind :: Int -> [Card] -> Bool
nOfAKind n hand = n <= maximum (map (\x -> length (intersect (wut hand) [x])) [1 .. 13])
	where wut :: [Card] -> [Int]
	      wut hand = map (\(Cd s n) -> n) hand