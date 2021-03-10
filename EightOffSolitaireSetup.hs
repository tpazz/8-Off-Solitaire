module EightOffSolitaire where

  import MergeSort
  import Random10_52
  import System.Random

  {-*****************EOBoard******************
    F    R    R    R    R    R    R    R    R

    F   [C]  [C]  [C]  [C]  [C]  [C]  [C]  [C]

    F

    F
    *****************************************-}

  -- Test dummy board
  myEOBoard :: EOBoard
  myEOBoard = ([],
    [[(Ace,Hearts),(Nine,Hearts),(Jack,Clubs),(Nine,Spades),(Two,Hearts)],
    [(Ace,Spades),(Two,Diamonds),(Three,Clubs),(Seven,Hearts),(Ten,Clubs),(Three,Hearts),(Six,Spades)],
    [(Seven,Spades),(Four,Hearts),(Queen,Clubs),(Four,Diamonds),(Six,Diamonds),(King,Spades)],
    [(Seven,Clubs),(Four,Clubs),(Ten,Diamonds),(King,Hearts),(Five,Clubs)],
    [(Two,Spades),(Three,Spades),(King,Clubs),(Jack,Hearts),(Five,Spades),(Queen,Hearts)],
    [(Two,Clubs),(Six,Hearts),(Ten,Hearts),(Jack,Spades),(Eight,Spades),(Queen,Diamonds)],
    [(Nine,Clubs),(Three,Diamonds),(Eight,Clubs),(Eight,Hearts),(Six,Clubs)],
    [(Eight,Diamonds),(Five,Hearts),(Ten,Spades)]],
    [(Ace,Diamonds),(Ace,Clubs),(Nine,Diamonds),(Four,Spades)])


  -- Datatypes
  data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
             Ten | Jack | Queen | King            deriving (Ord, Eq, Show, Enum)
  data Suit = Spades | Clubs | Hearts | Diamonds  deriving (Ord, Eq, Show, Enum)

  -- Constants & Utilities
  type Card = (Pip, Suit)
  type Deck = [Card]
  type Foundations = [Card] -- only need to know top card on foundations
  type Columns = [[Card]]
  type Reserves = [Card]
  type EOBoard = (Foundations, Columns, Reserves)

  -- Populates a deck with 52 cards
  pack :: Deck
  pack = [(p, s) | p <- [Ace ..], s <- [Spades ..]]

  -- Takes a card and returns true if Ace
  isAce :: Card -> Bool
  isAce (p, _) = (p == Ace)

  -- Takes a card an returns true if King
  isKing :: Card -> Bool
  isKing (p, _) = (p == King)

  -- Takes a card and returns the successor to that card
  sCard :: Card -> Card
  sCard (p, s) = (succ(p), s)

  -- Takes a card and returns the predecessor to that card
  pCard :: Card -> Card
  pCard (p, s) = (pred(p), s)

  -- Takes a deck and returns a shuffled deck
  shuffle :: Deck -> Deck
  shuffle d = let
          r = take 52 (randoms (mkStdGen 420) :: [Int])
          z = zip d r
          s = mergesort (\ (_, n1) (_, n2) -> n1 < n2) z
          in map fst s

  -- Takes a deck and returns a shuffled EOBoard
  eODeal :: Deck -> EOBoard
  eODeal d = let
         s = shuffle d
         f = []
         c = eODealA (take 48 s) -- split first 48 cards into columns
         r = drop 48 s
         in (f,c,r)

  -- eODealA fnc that takes a deck and returns a list of 6 card decks
  eODealA :: Deck -> [Deck]
  eODealA [] = []
  eODealA c = take 6 c : eODealA (drop 6 c)

  -- Takes an EOBoard and returns an EOBoard with all possible cards placed on their foundations
  toFoundations :: EOBoard -> EOBoard
  toFoundations x -- recursively called until no change in EOBoard
    | (x /= toFoundationsA x) = toFoundations (toFoundationsA x)
    | (x /= toFoundationsAA x) = toFoundations (toFoundationsAA x)
    | (x /= toFoundationsAAA x) = toFoundations (toFoundationsAAA x)
    | (x /= toFoundationsAAAA x) = toFoundations (toFoundationsAAAA x)
    | otherwise = x

  -- Takes an EOBoard and returns an EOBoard with column Ace cards --> foundations
  toFoundationsA :: EOBoard -> EOBoard
  toFoundationsA (f,c,r) = (f ++ (filter isAce (map head c)) , -- append head column's to foundations if Ace
    (map (\ x @ (h : t) -> if (isAce h) then t else x) c), r) -- return tail to column if head is Ace

  -- Takes an EOBoard and returns an EOBoard with reserve Ace cards --> foundations
  toFoundationsAA :: EOBoard -> EOBoard
  toFoundationsAA (f,c,r) = (f ++ (filter isAce r), c, -- append Ace's to foundations
    ([h | h <- r, ((isAce h) == False)])) -- append non-Ace cards to reserves

  -- Takes an EOBoard and returns an EOBoard with column cards --> foundations
  toFoundationsAAA :: EOBoard -> EOBoard
  toFoundationsAAA (f,c,r) = ((map (\ x -> if elem (sCard x) (map head c)
    then (sCard x) else x) f), -- set foundation card to it's successor if the card exists in head's of column
    (map (\ x @ (h : t) -> if elem (pCard h) f then t else x) c), r) -- return tail to column if head is predecessor to foundation card

  -- Takes an EOBoard and returns an EOBoard with reserve cards --> foundations
  toFoundationsAAAA :: EOBoard -> EOBoard
  toFoundationsAAAA (f,c,r) = ((map (\ x -> if elem (sCard x) r -- set foundation card to its successor if the card exists in reserves
    then (sCard x) else x) f), c, ([h | h <- r, ((elem (pCard h) f) == False)])) -- append non-predecessor cards to reserves
