----------------------------------------------------------------------
--   A Module for Playing Cards  (for use in Homework 6)
----------------------------------------------------------------------

module Cards where

--
--  First, introduce some new types: Suit and Card
--

--  There are four possible suits: clubs, diamonds, hearts, and spades
data Suit = Clubs | Diamonds | Hearts | Spades
	    deriving (Eq, Ord, Enum, Show)

--
--  Each card has both a suit and value
--
--      Idea: Value of (Cd suit n) is:
--            Ace,  if n=1
--             n,   if 2 <= n <= 10
--           Jack,  if n=11
--           Queen,  if n=12
--           King,  if n=13
--
--    Beware: this definition allows "bogus" cards, such as Cd Hearts 57  
--       (We won't worry about it, though...)
--
data Card = Cd Suit Int 
            deriving (Eq,Show)



----------------------------------------------------------------------

--
--  Three sample hands, plus a full deck
--
hand1, hand2, hand3, fulldeck :: [Card]

hand1 = [Cd Spades 1, Cd Hearts 3, Cd Clubs 2, Cd Clubs 9, Cd Hearts 1] 

hand2 = [Cd Diamonds 13, Cd Clubs 13, Cd Hearts 13, Cd Spades 1]

hand3 = [Cd Hearts 3, Cd Hearts 8, Cd Diamonds 10, Cd Diamonds 11, Cd
            Clubs 13, Cd Hearts 13, Cd Spades 1] 

fulldeck = [Cd s n | s <- [Clubs .. Spades], n <-[1..13]]

----------------------------------------------------------------------

-- Another way of displaying the cards

displayCard :: Card -> String
displayCard (Cd suit n) = (theVal !! (n-1)) ++ " " ++ show suit
  where
  theVal = ["Ace","2","3","4","5","6","7","8","9","10","Jack","Queen","King"]

displayPack :: [Card] -> [String]
displayPack pack = [ displayCard c | c <- pack]

----------------------------------------------------------------------

