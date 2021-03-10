module EOIO where

{- IO for EO solitaire
   display an EOBoard
   display a list of EOBoards
play a game, displaying successive moves -}

 import MySolitaire
 import Data.Maybe
 import Data.List
 import System.Random
 import Debug.Trace

 -- Takes an EOBoard and returns a list of all potential moves that are one move away
 findMoves :: EOBoard -> [EOBoard]
 findMoves brd@(f,c,r) = let
   cH = [h | (h : t) <- filter (not.null) c]
   lReserveToColumn = [m | m <- r, elem (sCard m) cH] -- populate potential moves for R -> C
   lColumnToColumn = [m | m <- cH, elem (sCard m) cH] -- populate potential moves for C -> C
   mReservesToColumn = filter (/= brd) (map (\ x -> reserveToColumn x brd) lReserveToColumn)
   mColumnToColumn = filter (/= brd) (map (\ x -> columnToColumn x brd) lColumnToColumn)
   mColumnToReserve = filter (/= brd) (map (\ x -> columnToReserve x brd) cH) -- column heads are the potential moves for C -> R
   in mReservesToColumn ++ mColumnToReserve ++ mColumnToColumn

 -- Takes an EOBoard and calls findMoves. It returns the best move depending on the board weight
 chooseMove :: EOBoard -> Maybe EOBoard
 chooseMove brd
   | (not.null) potentialMoves = Just bestMove
   | otherwise = Nothing
    where potentialMoves = map toFoundations (findMoves brd)
          scoredMoves = map weightedBoard potentialMoves -- add weight 'score' to each board
          zMoves = zip potentialMoves scoredMoves
          sortedMoves = sortBy (\ (_, n1) (_, n2) -> compare n1 n2) zMoves
          orderedMoves = map fst sortedMoves -- order by highest scoring board
          bestMove = head orderedMoves -- select highest scoring board for next move

 -- Takes an EOBoard and returns a score after playing a full game
 eOGame :: EOBoard -> Int
 eOGame brd@(f,c,r)
   | isNothing board = sum (map length f)
   | otherwise = eOGame (resMaybe board)
    where board = chooseMove brd

 -- Takes an Int that will be used as an initial random seed to play 100 games.
 -- Will return the number of wins and average score
 eOExpt :: Int -> (Int,Int)
 eOExpt seed = let
   initialBoards = map eODeal (take 100 (randoms (mkStdGen seed) :: [Int]))
   totalScoreBoards = map eOGame initialBoards
   totalWins = [w | w <- totalScoreBoards, w == 52]
   averageScore = (sum totalScoreBoards) `div` (52 * 100)
   in (length totalWins,averageScore)

 -- Takes a Card from the Reserves and updates the column it will be appended to
 -- If the card is king and there is an empty column, the empty column will be updated with the king card
 reserveToColumn :: Card -> EOBoard -> EOBoard
 reserveToColumn crd (f,c,r)
   | (length c < 8) && (isKing crd) = (f,[crd]:c,nR)
   | otherwise = (f,nC,nR)
    where cols = filter (not.null) c
          nR = filter (/= crd) r
          nC = map (\ col@(h : t) -> if (pCard crd == h) then (crd : col) else col) cols

 -- Takes a Card from one column and updates another column it will be appended to
 -- If the card is king and there is an empty column, the empty column will be updated with the king card
 columnToColumn :: Card -> EOBoard -> EOBoard
 columnToColumn crd (f,c,r)
   | (length c < 8) && (isKing crd) = (f,[crd]:c,r)
   | otherwise = (f,nC,r)
    where cols = filter (not.null) c
          delC = map (\ col@(h : t) -> if (h == crd) then t else col) cols
          nC = map (\ col@(h : t) -> if (pCard crd == h) then (crd : col) else col) delC

 -- Takes a Card from the head of a column and appends it to the Reserves (if enough space)
 -- Card not moved if Reserves already has 5 taken cells
 columnToReserve :: Card -> EOBoard -> EOBoard
 columnToReserve crd brd@(f,c,r)
   | consecCards crd c = brd
   | length r < 5 = (f,nC,nR)
   | otherwise = brd
    where cols = filter (not.null) c
          nC = map (\ col@(h : t) -> if (h == crd) then t else col) cols
          nR = crd:r

 -- Function used by columnToReserve to check for consecutive pairs of cards in columns.
 -- To ensure that cards do not swap between Reserves and Columns indefinitely.
 consecCards :: Card -> Columns -> Bool
 consecCards crd [] = False
 consecCards crd (h:t)
   | (length h > 1) && (pCard crd == h!!1) = True
   | otherwise = consecCards crd t

 -- Calculates the weight of the EOBoard by summing the face card values of Foundations and Reserves
 weightedBoard :: EOBoard -> Int
 weightedBoard (f,c,r) = sum (map cardPipValue f) + reservesPenalty r

 -- Weighting function that penalises score of a board for using up reserve cells
 reservesPenalty :: Reserves -> Int
 reservesPenalty [] = 0
 reservesPenalty r
   | l == 8 = -3
   | l == 7 = -2
   | l == 6 = -1
   | otherwise = 0
    where l = length r

 -- Used by weightedBoard to calculate the (pip) value of each card
 cardPipValue :: Card -> Int
 cardPipValue (p,s)
   | p == Ace = 1
   | p == Two = 2
   | p == Three = 3
   | p == Four = 4
   | p == Five = 5
   | p == Six = 6
   | p == Seven = 7
   | p == Eight = 8
   | p == Nine = 9
   | p == Ten = 10
   | p == Jack = 11
   | p == Queen = 12
   | p == King = 13

 ----------------------------------------------------------
 -- display an EOBoard
 displayEOB :: EOBoard -> IO String

 displayEOB (fnds,cols,res) = do
  let colStr = colsToString cols
  putStr "EOBoard\nFoundations  "
  putStrLn (show fnds)
  putStr  "Columns"
  putStr colStr
  putStr "\n\nReserve     "
  putStrLn (show res)
  putStr "\n---------------------------------------------\n"
  return ""

 colsToString :: Columns->String -- prepare String to print columns on separate lines

 colsToString cols =
  foldr (++) "" ["\n             "++(show col) |col<-cols]

-----------------------------------------------------------------------

-- display a list of EOBoards

 displayEOBList :: [EOBoard]-> IO String

 displayEOBList eobl =  -- @ notation doesn't seem to work correctly
  do
   if (null eobl) then do (return "")
                  else do
                        displayEOB (head eobl)
                        displayEOBList (tail eobl)


-----------------------------------------------------------------

 --scoreBoard
 -- score is number of cards on foundations
 -- return a String for display

 scoreBoard :: EOBoard-> String
 scoreBoard (fnds, cols, res) = "A LOSS: SCORE  " ++ (show (52- (length res) - (foldr (+) 0 (map length cols))))

 -----------------------------------------------------------------------------
 -- play a game given initial board
 -- assuming a fn chooseMove :: EOBoard ->Maybe EOBoard
 -- & that toFoundations is handled outside

 displayEOGame :: EOBoard ->IO String

 displayEOGame b = do
  let (fnds,cols,res) = b -- apparently can't do this with @
  if ((null cols)&&(null res)) -- if cols & reserve empty its a win
     then return "A WIN"
     else
      do
       displayEOB b -- display given board
       let res = chooseMove b
       if (isJust res) then
               do
                let nb = resMaybe res
                displayEOGame nb
              else
               do
                 let score = scoreBoard b
                 return score

 ------------------------------------------------
 -- Maybe helper
 resMaybe :: (Maybe a) -> a
 resMaybe (Just x) = x
