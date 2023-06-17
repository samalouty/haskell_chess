type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])
type Array2D a = [[a]]

-- print visualizeBoard on the console using this:    putStrLn(visualizeBoard setBoard)  to see output on different lines 

insertAt :: Int -> Int -> a -> Array2D a -> Array2D a
insertAt row col newValue array = take row array ++
                                  [take col (array !! row) ++ [newValue] ++ drop (col+1) (array !! row)] ++
                                  drop (row+1) array		  

setBoard :: Board 
setBoard = (White,
			[R ('h',1),N ('g',1),B ('f',1),K ('e',1),Q ('d',1),B ('c',1),N ('b',1),R ('a',1),P ('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P ('c',2),P ('b',2),P ('a',2)]
     		,[R ('h',8),N ('g',8),B ('f',8),K ('e',8),Q ('d',8),B ('c',8),N ('b',8),R ('a',8),P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P ('a',7)])

boardRows :: [[String]]
boardRows = [[" ", " ", "a", "   ", "b", "  ", "c", "  ", "d", "  ", "e", "  ", "f", "  ", "g", "  ", "h", " "],
                 ["8", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|"],
                 ["7", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|"],
                 ["6", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|"],
                 ["5", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|"],
                 ["4", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|"],
                 ["3", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|"],
                 ["2", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|"],
                 ["1", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|", "  ", "|"]]

letterToIndex :: Char -> Int
letterToIndex 'h' = 16
letterToIndex 'g' = 14
letterToIndex 'f' = 12
letterToIndex 'e' = 10
letterToIndex 'd' = 8
letterToIndex 'c' = 6
letterToIndex 'b' = 4
letterToIndex 'a' = 2

getColumn :: Piece -> Int
getColumn (P (n, _)) = letterToIndex n
getColumn (N (n, _)) = letterToIndex n
getColumn (B (n, _)) = letterToIndex n
getColumn (R (n, _)) = letterToIndex n
getColumn (Q (n, _)) = letterToIndex n
getColumn (K (n, _)) = letterToIndex n

getColumn2 :: Piece -> Int
getColumn2 (P (n, _)) = charToInt n
getColumn2 (N (n, _)) = charToInt n
getColumn2 (B (n, _)) = charToInt n
getColumn2 (R (n, _)) = charToInt n
getColumn2 (Q (n, _)) = charToInt n
getColumn2 (K (n, _)) = charToInt n

getPieceNumber :: Piece -> Int
getPieceNumber (P (_, n)) = 9-n
getPieceNumber (N (_, n)) = 9-n
getPieceNumber (B (_, n)) = 9-n
getPieceNumber (R (_, n)) = 9-n
getPieceNumber (Q (_, n)) = 9-n
getPieceNumber (K (_, n)) = 9-n

getPieceNumber2 :: Piece -> Int
getPieceNumber2 (P (_, n)) = n
getPieceNumber2 (N (_, n)) = n
getPieceNumber2 (B (_, n)) = n
getPieceNumber2 (R (_, n)) = n
getPieceNumber2 (Q (_, n)) = n
getPieceNumber2 (K (_, n)) = n

pieceToString :: Piece -> String
pieceToString (P _) = "P"
pieceToString (K _) = "K"
pieceToString (N _) = "N"
pieceToString (Q _) = "Q"
pieceToString (R _) = "R"
pieceToString (B _) = "B"

insertBlackPieces [] b = b
insertBlackPieces (x:xs) b = insertAt (getPieceNumber x) (getColumn x) (pieceToString x ++ "B") b' where b' = insertBlackPieces xs b

insertWhitePieces [] b = b
insertWhitePieces (x:xs) b = insertAt (getPieceNumber x) (getColumn x) (pieceToString x ++ "W") b' where b' = insertWhitePieces xs b

convertToString :: [[String]] -> String
convertToString = unlines . map (concat . addSeparators)
  where
    addSeparators (x:xs) = x : "" : xs

visualizeBoard :: Board -> String
visualizeBoard (player, whitePieces, blackPieces) =
 convertToString (insertBlackPieces blackPieces (insertWhitePieces whitePieces boardRows)) ++ "\n\nTurn: " ++ playerToString player

playerToString  White = "White"
playerToString  Black = "Black"
 
suggestMove :: Piece -> Board -> [Location]
suggestMove p b = convertToLocation (suggestMove2 p b)

convertToLocation []=[]
convertToLocation ((h, t):xs) = (((intToChar h), t):(convertToLocation xs))

suggestMove2 (P (x, y)) board@(player, whitePieces, blackPieces) = if (P (x, y)) `elem` whitePieces then
      if y < 8 && y/=2 && ((isOccupied (player, whitePieces, blackPieces) (P (x, y + 1))) == False)
        then ([((charToInt x), y + 1)] ++ whitePawnCapture (charToInt x) (y) (board) (P(x,y)))
        else if y == 2 && y<8 && ((isOccupied (player, whitePieces, blackPieces) (P (x, y + 1))) == False) && ((isOccupied (player, whitePieces, blackPieces) (P (x, y + 2))) == False)
               then ([((charToInt x), y + 1), ((charToInt x), y + 2)] ++ whitePawnCapture (charToInt x) (y) (board) (P(x,y)))
               else if y== 2 && y<8 &&((isOccupied (player, whitePieces, blackPieces) (P (x, y + 1))) == False) && ((isOccupied (player, whitePieces, blackPieces) (P (x, y + 2))) == True)
			        then ([((charToInt x), y + 1)] ++ whitePawnCapture (charToInt x) (y) (board) (P(x,y)))else (whitePawnCapture (charToInt x) (y) (board) (P(x,y)))
  else
      if y > 1 && y/=7 && ((isOccupied (player, whitePieces, blackPieces) (P (x, y - 1))) == False)
        then ([((charToInt x), y - 1)] ++ blackPawnCapture (charToInt x) (y) (board) (P(x,y)))
        else if y == 7 && y>1 && ((isOccupied (player, whitePieces, blackPieces) (P (x, y - 1))) == False) && ((isOccupied (player, whitePieces, blackPieces) (P (x, y - 2))) == False)
               then ([((charToInt x), y - 1), ((charToInt x), y - 2)] ++ blackPawnCapture (charToInt x) (y) (board) (P(x,y)))
                else if y== 7 && y>1 &&((isOccupied (player, whitePieces, blackPieces) (P (x, y - 1))) == False) && ((isOccupied (player, whitePieces, blackPieces) (P (x, y - 2))) == True)
			        then ([((charToInt x), y - 1)]++ blackPawnCapture (charToInt x) (y) (board) (P(x,y))) else (blackPawnCapture (charToInt x) (y) (board) (P(x,y)))
					
suggestMove2 (N (x, y)) (player, whitePieces, blackPieces) =
  let possibleMoves =
        [ ((charToInt x) + 1, y + 2),
          ((charToInt x) + 1, y - 2),
          ((charToInt x) - 1, y + 2),
          ((charToInt x) - 1, y - 2),
          ((charToInt x) + 2, y + 1),
          ((charToInt x) + 2, y - 1),
          ((charToInt x) - 2, y + 1),
          ((charToInt x) - 2, y - 1)
        ]
   in filter isValidMove possibleMoves
  where
    isValidMove (x', y') =  x' >= 0 && x' <= 7 && y' >= 1 && y' <= 8 && (isOccupied (player, whitePieces, blackPieces) (N (intToChar x',y')) == False || isOccupiedDiffColor (player,whitePieces,blackPieces) (N (x,y)) (x',y'))   
suggestMove2 (K (x, y)) (player, whitePieces, blackPieces) =
  let possibleMoves =
        [ ((charToInt x) + 1, y),
          ((charToInt x) - 1, y),
          ((charToInt x), y + 1),
          ((charToInt x), y - 1),
          ((charToInt x) + 1, y + 1),
          ((charToInt x) + 1, y - 1),
          ((charToInt x) - 1, y + 1),
          ((charToInt x) - 1, y - 1)
        ]
   in filter isValidMove possibleMoves
  where
    isValidMove (x', y') =  x' >= 0 &&  x' <= 7 && y' >= 1 && y' <= 8 && (isOccupied (player, whitePieces, blackPieces) (K (intToChar x',y')) == False || isOccupiedDiffColor (player,whitePieces,blackPieces) (K (x,y)) (x',y'))                                                

suggestMove2 (Q (x, y)) (player, whitePieces, blackPieces) =
  let diagonalUR = takeWhile (\(x',y') -> not(isOccupied (player, whitePieces, blackPieces) (R (intToChar x', y')))) (diagonalUR2 (charToInt x) y)
      diagonalDL = takeWhile (\(x',y') -> not(isOccupied (player, whitePieces, blackPieces) (R (intToChar x', y')))) (diagonalDL2 (charToInt x) y)
      diagonalUL = takeWhile (\(x',y') -> not(isOccupied (player, whitePieces, blackPieces) (R (intToChar x', y')))) (diagonalUL2 (charToInt x) y)
      diagonalDR = takeWhile (\(x',y') -> not(isOccupied (player, whitePieces, blackPieces) (R (intToChar x', y')))) (diagonalDR2 (charToInt x) y)
      leftMoves = takeWhile (\(x',_) -> not(isOccupied (player, whitePieces, blackPieces) (Q (intToChar x', y)))) (leftMoves2 (charToInt x) y)
      upMoves = takeWhile (\(_,y') -> not(isOccupied (player, whitePieces, blackPieces) (Q (x, y')))) (upMoves2 (charToInt x) y)
      rightMoves = takeWhile (\(x',_) -> not(isOccupied (player, whitePieces, blackPieces) (Q (intToChar x', y)))) (rightMoves2 (charToInt x) y)
      downMoves = takeWhile (\(_,y') -> not(isOccupied (player, whitePieces, blackPieces) (Q (x, y')))) (downMoves2 (charToInt x) y)
  in upMoves ++ leftMoves  ++ rightMoves  ++ downMoves 
  ++ captureL (leftMoves) (player, whitePieces, blackPieces) (Q (x,y)) ++ captureR (rightMoves) (player, whitePieces, blackPieces) (Q (x,y)) 
  ++ captureU (upMoves) (player, whitePieces, blackPieces) (Q (x,y)) ++ captureD (downMoves) (player, whitePieces, blackPieces) (Q (x,y)) 
  ++ diagonalUR ++ diagonalDL ++ diagonalUL ++ diagonalDR 
  ++ captureUR (diagonalUR) (player, whitePieces, blackPieces) (Q (x,y)) ++ captureDL (diagonalDL) (player, whitePieces, blackPieces) (Q (x,y)) 
  ++ captureUL (diagonalUL) (player, whitePieces, blackPieces) (Q (x,y)) ++ captureDR (diagonalDR) (player, whitePieces, blackPieces) (Q (x,y))


suggestMove2 (R (x, y)) (player, whitePieces, blackPieces) =
  let leftMoves = takeWhile (\(x',_) -> not(isOccupied (player, whitePieces, blackPieces) (R (intToChar x', y)))) (leftMoves2 (charToInt x) y)
      upMoves = takeWhile (\(_,y') -> not(isOccupied (player, whitePieces, blackPieces) (R (x, y')))) (upMoves2 (charToInt x) y)
      rightMoves = takeWhile (\(x',_) -> not(isOccupied (player, whitePieces, blackPieces) (R (intToChar x', y)))) (rightMoves2 (charToInt x) y)
      downMoves = takeWhile (\(_,y') -> not(isOccupied (player, whitePieces, blackPieces) (R (x, y')))) (downMoves2 (charToInt x) y)
  in upMoves ++ leftMoves  ++ rightMoves  ++ downMoves 
  ++ captureL (leftMoves) (player, whitePieces, blackPieces) (R (x,y)) ++ captureR (rightMoves) (player, whitePieces, blackPieces) (R (x,y)) 
  ++ captureU (upMoves) (player, whitePieces, blackPieces) (R (x,y)) ++ captureD (downMoves) (player, whitePieces, blackPieces) (R (x,y))

suggestMove2 (B (x, y)) (player, whitePieces, blackPieces) =
  let diagonalUR = takeWhile (\(x',y') -> not(isOccupied (player, whitePieces, blackPieces) (R (intToChar x', y')))) (diagonalUR2 (charToInt x) y)
      diagonalDL = takeWhile (\(x',y') -> not(isOccupied (player, whitePieces, blackPieces) (R (intToChar x', y')))) (diagonalDL2 (charToInt x) y)
      diagonalUL = takeWhile (\(x',y') -> not(isOccupied (player, whitePieces, blackPieces) (R (intToChar x', y')))) (diagonalUL2 (charToInt x) y)
      diagonalDR = takeWhile (\(x',y') -> not(isOccupied (player, whitePieces, blackPieces) (R (intToChar x', y')))) (diagonalDR2 (charToInt x) y)
  in diagonalUR ++ diagonalDL ++ diagonalUL ++ diagonalDR 
  ++ captureUR (diagonalUR) (player, whitePieces, blackPieces) (B (x,y)) ++ captureDL (diagonalDL) (player, whitePieces, blackPieces) (B (x,y)) 
  ++ captureUL (diagonalUL) (player, whitePieces, blackPieces) (B (x,y)) ++ captureDR (diagonalDR) (player, whitePieces, blackPieces) (B (x,y))
  
whitePawnCapture :: Int -> Int -> Board -> Piece -> [(Int, Int)]

whitePawnCapture x y board p1 | x+1 <8 && y+1 <9 && x-1 >0 && isOccupiedDiffColor board p1 (x+1, y+1) && isOccupiedDiffColor board p1 (x-1, y+1) = [(x+1, y+1),(x-1, y+1)]
							  | x+1 <8 && y+1 <9 && isOccupiedDiffColor board p1 (x+1, y+1) = [(x+1, y+1)]
							  | x-1 >0 && y+1 <9 && isOccupiedDiffColor board p1 (x-1, y+1) = [(x-1, y+1)]
							  | otherwise = []
	
blackPawnCapture :: Int -> Int -> Board -> Piece -> [(Int, Int)]
	
blackPawnCapture x y board p1   | x+1 <8 && y-1 > 0 && x-1 > 0  && isOccupiedDiffColor board p1 (x+1, y-1) && isOccupiedDiffColor board p1 (x-1, y-1) = [(x+1, y-1),(x-1, y-1)]
								| x+1 <8 && y-1 > 0 && isOccupiedDiffColor board p1 (x+1, y-1) = [(x+1, y-1)]
								| x-1 > 0 && y-1 > 0 && isOccupiedDiffColor board p1 (x-1, y-1) = [(x-1, y-1)]
								| otherwise = []  
  
leftMoves2 :: Int -> Int -> [(Int,Int)]
leftMoves2 x y= if x>0 then (x-1,y):(leftMoves2 (x-1) y) else []

rightMoves2 :: Int -> Int -> [(Int,Int)]
rightMoves2 x y= if x<7 then (x+1,y):(rightMoves2 (x+1) y) else []

upMoves2 :: Int -> Int -> [(Int,Int)]
upMoves2 x y= if y<8 then (x,y+1):(upMoves2 x (y+1)) else []

downMoves2 :: Int -> Int -> [(Int,Int)]
downMoves2 x y= if y>1 then (x,y-1):(downMoves2 x (y-1)) else []

captureL :: [(Int,Int)] -> Board -> Piece -> [(Int,Int)]
captureL l board p1 = 
				if l == [] then
					if getFirst (locationPieceNum (locationPiece p1)) /= (-1)  && isOccupiedDiffColor board p1 (locationPieceNum (locationPiece p1))
							then [locationPieceNum (locationPiece p1)] else []
				else if (getFirst(minusL (last l)) /= (-1)) && (isOccupiedDiffColor board p1 (minusL (last l))) 
					then  [minusL (last l)] else []

captureR :: [(Int,Int)] -> Board -> Piece -> [(Int,Int)]
captureR l board p1 = if l == [] then 
                if getFirst (locationPieceNum2 (locationPiece p1)) /= 8  && isOccupiedDiffColor board p1 (locationPieceNum2 (locationPiece p1))
							then [locationPieceNum2 (locationPiece p1)] else []
				else if (getFirst (minusR (last l)) /= 8) && (isOccupiedDiffColor board p1 (minusR (last l))) 
                    then  [minusR (last l)] else []

captureU :: [(Int,Int)] -> Board -> Piece -> [(Int,Int)]
captureU l board p1 = if l == [] then 
                if getLast (locationPieceNum3 (locationPiece p1)) /= 9  && isOccupiedDiffColor board p1 (locationPieceNum3 (locationPiece p1))
							then [locationPieceNum3 (locationPiece p1)] else []
				else if (getLast (minusU (last l)) /= 9) && (isOccupiedDiffColor board p1 (minusU (last l))) 
                    then  [minusU (last l)] else []

captureD :: [(Int,Int)] -> Board -> Piece -> [(Int,Int)]
captureD l board p1 = if l == [] then 
                if getLast (locationPieceNum4 (locationPiece p1)) /= 0  && isOccupiedDiffColor board p1 (locationPieceNum4 (locationPiece p1))
							then [locationPieceNum4 (locationPiece p1)] else []
				else if (getLast (minusD (last l)) /= 0) && (isOccupiedDiffColor board p1 (minusD (last l))) 
                    then  [minusD (last l)] else []

minusL :: (Int,Int) -> (Int,Int)
minusL (x,y) = (x-1,y)

minusR :: (Int,Int) -> (Int,Int)
minusR (x,y) = (x+1,y)

minusU :: (Int,Int) -> (Int,Int)
minusU (x,y) = (x,y+1)

minusD :: (Int,Int) -> (Int,Int)
minusD (x,y) = (x,y-1)

getFirst (x,y) = x
getLast (x,y) = y

locationPieceNum (x, y) = ((charToInt x) - 1, y)
locationPieceNum2 (x, y) = ((charToInt x) + 1, y)
locationPieceNum3 (x, y) = (charToInt x, y + 1)
locationPieceNum4 (x, y) = (charToInt x, y - 1)

diagonalUR2 :: Int -> Int -> [(Int,Int)]
diagonalUR2 x y= if x<7 && y<8 then (x+1,y+1):(diagonalUR2 (x+1) (y+1)) else []

diagonalDL2:: Int -> Int -> [(Int,Int)]
diagonalDL2 x y= if x>0 && y>1 then (x-1,y-1):(diagonalDL2 (x-1) (y-1)) else []

diagonalUL2 :: Int -> Int -> [(Int,Int)]
diagonalUL2 x y= if x>0 && y<8 then (x-1,y+1):(diagonalUL2 (x-1) (y+1)) else []

diagonalDR2 :: Int -> Int -> [(Int,Int)]
diagonalDR2 x y= if x<7 && y>1 then (x+1,y-1):(diagonalDR2 (x+1) (y-1)) else []

captureUR :: [(Int,Int)] -> Board -> Piece -> [(Int,Int)]
captureUR l board p1 = if l == [] then 
                if (getLast (locationPieceD (locationPiece p1)) /= 9 && getFirst (locationPieceD (locationPiece p1)) /= 8)  && isOccupiedDiffColor board p1 (locationPieceD (locationPiece p1))
							then [locationPieceD (locationPiece p1)] else []
				else if (getLast (minusD1 (last l)) /= 9 && (getFirst (minusD1 (last l))) /= 8) && (isOccupiedDiffColor board p1 (minusD1 (last l))) 
                    then  [minusD1 (last l)] else []

captureDL :: [(Int,Int)] -> Board -> Piece -> [(Int,Int)]
captureDL l board p1 = if l == [] then 
                if (getLast (locationPieceD2 (locationPiece p1)) /= 0 && ((getFirst (locationPieceD2 (locationPiece p1))) /= (-1)))  && isOccupiedDiffColor board p1 (locationPieceD2 (locationPiece p1))
							then [locationPieceD2 (locationPiece p1)] else []
				else if (getLast (minusD2 (last l)) /= 0 && (getFirst (minusD2 (last l))) /= (-1)) && (isOccupiedDiffColor board p1 (minusD2 (last l))) 
                    then  [minusD2 (last l)] else []

captureUL :: [(Int,Int)] -> Board -> Piece -> [(Int,Int)]
captureUL l board p1 = if l == [] then 
                if (getLast (locationPieceD3 (locationPiece p1)) /= 9 && ((getFirst (locationPieceD3 (locationPiece p1))) /= (-1)))  && isOccupiedDiffColor board p1 (locationPieceD3 (locationPiece p1))
							then [locationPieceD3 (locationPiece p1)] else []
				else if ((getLast (minusD3 (last l)) /= 9) && ((getFirst (minusD3 (last l))) /= (-1))) && (isOccupiedDiffColor board p1 (minusD3 (last l))) 
                    then  [minusD3 (last l)] else []
					
captureDR :: [(Int,Int)] -> Board -> Piece -> [(Int,Int)]
captureDR l board p1 = if l == [] then 
                if (getLast (locationPieceD4 (locationPiece p1)) /= 0 && ((getFirst (locationPieceD4 (locationPiece p1))) /= 8))  && isOccupiedDiffColor board p1 (locationPieceD4 (locationPiece p1))
							then [locationPieceD4 (locationPiece p1)] else []
				else if ((getLast (minusD4 (last l)) /= 0) && ((getFirst (minusD4 (last l))) /= 8)) && (isOccupiedDiffColor board p1 (minusD4 (last l))) 
                    then  [minusD4 (last l)] else []
					
minusD1 :: (Int,Int) -> (Int,Int)
minusD1 (x,y) = (x+1,y+1)

minusD2 :: (Int,Int) -> (Int,Int)
minusD2 (x,y) = (x-1,y-1)

minusD3 :: (Int,Int) -> (Int,Int)
minusD3 (x,y) = (x-1,y+1)

minusD4:: (Int,Int) -> (Int,Int)
minusD4 (x,y) = (x+1,y-1)

locationPieceD (x, y) = (((charToInt x) +1), y+1)
locationPieceD2 (x, y) = (((charToInt x) -1), y-1)
locationPieceD3 (x, y) = ((charToInt x) -1, y+1)
locationPieceD4 (x, y) = ((charToInt x) +1, y-1)

charToInt :: Char -> Int
charToInt 'a'=0
charToInt 'b'=1
charToInt 'c'=2
charToInt 'd'=3
charToInt 'e'=4
charToInt 'f'=5
charToInt 'g'=6
charToInt 'h'=7

intToChar :: Int -> Char
intToChar 0='a'
intToChar 1='b'
intToChar 2='c'
intToChar 3='d'
intToChar 4='e'
intToChar 5='f'
intToChar 6='g'
intToChar 7='h'

isOccupiedDiffColor :: Board -> Piece -> (Int,Int) -> Bool
isOccupiedDiffColor board p1 (x',y') = isOccupied board (K (intToChar x',y')) && (diffColor p1 (pieceLocation (intToChar x',y') board) board )

isOccupied :: Board -> Piece -> Bool
isOccupied (_, whitePieces, blackPieces) piece =
  tupleExists (locationPiece piece) (locationsWhite whitePieces blackPieces) || tupleExists (locationPiece piece) (locationsBlack whitePieces blackPieces)

locationsWhite :: [Piece] -> [Piece] -> [Location]
locationsWhite [] _ = []
locationsWhite (piece:ts) blackPieces =
  locationPiece piece : locationsWhite ts blackPieces
  
locationsBlack :: [Piece] -> [Piece] -> [Location]
locationsBlack _ [] = []
locationsBlack whitePieces (piece:ts) =
  locationPiece piece : locationsBlack whitePieces ts  

tupleExists :: Location -> [Location] -> Bool
tupleExists tuple array = tuple `elem` array

locationPiece :: Piece -> Location 
locationPiece (P (x,y)) = (x,y)
locationPiece (N (x,y)) = (x,y)
locationPiece (B (x,y)) = (x,y)
locationPiece (R (x,y)) = (x,y)
locationPiece (K (x,y)) = (x,y)
locationPiece (Q (x,y)) = (x,y)

pieceLocation :: Location -> Board -> Piece
pieceLocation loc (player,[],(h:t)) = if loc == (locationPiece h) then h else pieceLocation loc (player,[],t) 
pieceLocation loc (player,(h:t),[]) = if loc == (locationPiece h) then h else pieceLocation loc (player,t,[]) 
pieceLocation loc (player,(h1:t1),(h2:t2)) = if loc == (locationPiece h1) then h1
else if loc == (locationPiece h2) then h2
else pieceLocation loc (player,t1,t2)

diffColor :: Piece -> Piece -> Board -> Bool
diffColor p1 p2 (player,whitePieces,blackPieces) = if p1 `elem` whitePieces && p2 `elem` whitePieces then False 
else if p1 `elem` blackPieces && p2 `elem` blackPieces then False else True

isLegal:: Piece -> Board -> Location -> Bool
isLegal piece board loc = loc `elem` (suggestMove piece board) 

move:: Piece -> Location -> Board -> Board
move piece loc (player, whitePieces, blackPieces) = if piece `elem` whitePieces && player == Black 
then error "This is Black player's turn, White can't move." 
else if piece `elem` blackPieces && player == White 
then error "This is White player's turn, Black can't move." 
else if isLegal piece (player, whitePieces, blackPieces) loc == False 
then error ("Illegal move for piece " ++ pieceToString2 piece ++ ".")
else if piece `elem` whitePieces && loc `elem` (locationsBlack whitePieces blackPieces)
then (switch player, map (\p -> if p == piece then updateLocation p loc else p) whitePieces, delete (pieceLocation loc (player, whitePieces, blackPieces)) blackPieces)
else if piece `elem` whitePieces 
then (switch player, map (\p -> if p == piece then updateLocation p loc else p) whitePieces, blackPieces) 
else if piece `elem` blackPieces && loc `elem` (locationsWhite whitePieces blackPieces) 
then (switch player, delete (pieceLocation loc (player, whitePieces, blackPieces)) whitePieces, map (\p -> if p == piece then updateLocation p loc else p) blackPieces)
else (switch player, whitePieces, map (\p -> if p == piece then updateLocation p loc else p) blackPieces)

delete :: (Eq a) => a -> [a] -> [a]
delete y xs = delete'' y xs []
                 where delete'' _ [] _ = []
                       delete'' d (a:as) acc
                         | a == d    = reverse acc ++ as
                         | otherwise = delete'' d as (a:acc)

switch :: Player -> Player
switch x = if x==White then Black else White

pieceToString2 :: Piece -> String
pieceToString2 (P (x, y)) = "P ('" ++ [x] ++ "', " ++ show y ++ ")"
pieceToString2 (R (x, y)) = "R ('" ++ [x] ++ "', " ++ show y ++ ")"
pieceToString2 (N (x, y)) = "N ('" ++ [x] ++ "', " ++ show y ++ ")"
pieceToString2 (K (x, y)) = "K ('" ++ [x] ++ "', " ++ show y ++ ")"
pieceToString2 (B (x, y)) = "B ('" ++ [x] ++ "', " ++ show y ++ ")"
pieceToString2 (Q (x, y)) = "Q ('" ++ [x] ++ "', " ++ show y ++ ")"

updateLocation :: Piece -> Location -> Piece
updateLocation (P _) loc = P loc
updateLocation (N _) loc = N loc
updateLocation (K _) loc = K loc
updateLocation (Q _) loc = Q loc
updateLocation (R _) loc = R loc
updateLocation (B _) loc = B loc
