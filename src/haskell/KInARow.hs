-- K-in-a-Row (gravity/drop) game in Haskell
-- Comparison implementation for Prolog version
-- Author: Avdieienko Dmytro

module KInARow where

import Data.List  (transpose, maximumBy, nub)
import Data.Ord   (comparing)
import Data.Maybe (isNothing, fromMaybe)

-- ============================================================
-- Types
-- ============================================================

type Cell   = Maybe Player        -- Nothing=empty, Just X/O
data Player = X | O deriving (Eq, Show)

-- Board: 2D list (row-major), forbidden set, K
data Board = Board
  { rows      :: Int
  , cols      :: Int
  , winK      :: Int
  , forbidden :: [(Int,Int)]   -- 1-based (row,col)
  , cells     :: [[Cell]]      -- cells !! (r-1) !! (c-1)
  } deriving (Show)

-- Blocked cell sentinel (using a wrapper instead of Maybe for clarity)
blocked :: Cell
blocked = Nothing  -- forbidden cells stay Nothing; we track them separately

-- ============================================================
-- Board construction
-- ============================================================

-- | Create a fresh board. Precondition: rows>=5, cols>=6, k in [3..5].
newGame :: Int -> Int -> Int -> [(Int,Int)] -> Board
newGame r c k forb = Board r c k forb grid
  where
    grid = [ [ if (ri,ci) `elem` forb then Nothing else Nothing
               | ci <- [1..c] ]
             | ri <- [1..r] ]
    -- Note: forbidden cells start as Nothing too; we check the forbidden
    --       list when placing pieces.

-- ============================================================
-- Cell access
-- ============================================================

getCell :: Board -> Int -> Int -> Maybe Cell
getCell b r c
  | r < 1 || r > rows b || c < 1 || c > cols b = Nothing
  | otherwise = Just (cells b !! (r-1) !! (c-1))

setCell :: Board -> Int -> Int -> Cell -> Board
setCell b r c v = b { cells = updated }
  where
    updated = [ [ if ri==r && ci==c then v else cells b !! (ri-1) !! (ci-1)
                  | ci <- [1..cols b] ]
                | ri <- [1..rows b] ]

isForbidden :: Board -> Int -> Int -> Bool
isForbidden b r c = (r,c) `elem` forbidden b

-- ============================================================
-- Gravity
-- ============================================================

-- | Drop a piece in a column. Returns updated board and landing row.
dropPiece :: Board -> Int -> Player -> Maybe (Board, Int)
dropPiece b c p = case findBottom b c (rows b) of
  Nothing  -> Nothing
  Just row -> Just (setCell b row c (Just p), row)

findBottom :: Board -> Int -> Int -> Maybe Int
findBottom _ _ 0 = Nothing
findBottom b c r
  | isForbidden b r c = findBottom b c (r-1)
  | cells b !! (r-1) !! (c-1) == Nothing = Just r
  | otherwise = findBottom b c (r-1)

-- | Columns that still have room (top cell empty and not forbidden).
availableCols :: Board -> [Int]
availableCols b = filter available [1..cols b]
  where
    available c = not (isForbidden b 1 c)
                  && cells b !! 0 !! (c-1) == Nothing

-- ============================================================
-- Win detection
-- ============================================================

opponent :: Player -> Player
opponent X = O
opponent O = X

-- | True if the K-in-a-row condition is met at (r,c) for player p.
isWinner :: Board -> Int -> Int -> Player -> Bool
isWinner b r c p = any (checkDir b r c p (winK b)) dirs
  where dirs = [(0,1),(1,0),(1,1),(1,-1)]

checkDir :: Board -> Int -> Int -> Player -> Int -> (Int,Int) -> Bool
checkDir b r c p k (dr,dc) = total >= k
  where
    n1    = countDir b r c p dr dc
    n2    = countDir b r c p (-dr) (-dc)
    total = n1 + n2 - 1   -- avoid double-counting (r,c)

countDir :: Board -> Int -> Int -> Player -> Int -> Int -> Int
countDir b r c p dr dc
  | r < 1 || r > rows b || c < 1 || c > cols b = 0
  | isForbidden b r c = 0
  | getCell b r c /= Just (Just p) = 0
  | otherwise = 1 + countDir b (r+dr) (c+dc) p dr dc

-- | True if player has K in a row anywhere on the board.
winExists :: Board -> Player -> Bool
winExists b p = any (\(r,c) -> isWinner b r c p)
                [ (r,c) | r <- [1..rows b], c <- [1..cols b]
                        , getCell b r c == Just (Just p) ]

-- ============================================================
-- Heuristic evaluation
-- ============================================================

scoreFor :: Int -> Int
scoreFor 1 = 1
scoreFor 2 = 10
scoreFor 3 = 100
scoreFor 4 = 1000
scoreFor _ = 100000

-- | Evaluate a K-window (list of cells) for player p.
windowScore :: Player -> [Maybe Cell] -> Int
windowScore p window
  | oppCount > 0 = 0
  | otherwise    = scoreFor myCount
  where
    myCount  = length $ filter (== Just (Just p)) window
    oppCount = length $ filter (== Just (Just (opponent p))) window

-- | All K-length windows in all four directions.
allWindows :: Board -> Int -> [[ Maybe Cell ]]
allWindows b k =
    horizWins ++ vertWins ++ diagDR ++ diagUR
  where
    k' = k
    -- horizontal
    horizWins = [ map (\c -> getCell b r c) [c0..c0+k'-1]
                | r <- [1..rows b], c0 <- [1..cols b - k' + 1] ]
    -- vertical
    vertWins  = [ map (\r -> getCell b r c) [r0..r0+k'-1]
                | c <- [1..cols b], r0 <- [1..rows b - k' + 1] ]
    -- diagonal down-right
    diagDR    = [ map (\i -> getCell b (r0+i) (c0+i)) [0..k'-1]
                | r0 <- [1..rows b - k' + 1], c0 <- [1..cols b - k' + 1] ]
    -- diagonal up-right
    diagUR    = [ map (\i -> getCell b (r0-i) (c0+i)) [0..k'-1]
                | r0 <- [k'..rows b], c0 <- [1..cols b - k' + 1] ]

-- | Heuristic score for player p.
evalBoard :: Board -> Player -> Int
evalBoard b p = myPts - oppPts
  where
    k      = winK b
    ws     = allWindows b k
    myPts  = sum $ map (windowScore p)          ws
    oppPts = sum $ map (windowScore (opponent p)) ws

-- ============================================================
-- MinMax with Alpha-Beta (depth = half-moves)
-- ============================================================

-- | Alpha-Beta minimax. Returns score for current player.
minimax :: Board -> Int -> Int -> Int -> Player -> Bool -> Int
minimax b depth alpha beta player isMax
  | winExists b (opponent player) = 100000   -- previous move won
  | depth == 0                    = evalBoard b player
  | null moves                    = evalBoard b player
  | isMax = go moves alpha
  | otherwise = goMin moves beta
  where
    moves = availableCols b
    -- maximizer
    go [] curAlpha = curAlpha
    go (c:cs) curAlpha =
      case dropPiece b c player of
        Nothing       -> go cs curAlpha
        Just (nb, _)  ->
          let childScore = minimax nb (depth-1) curAlpha beta (opponent player) False
              newAlpha   = max curAlpha childScore
          in if newAlpha >= beta
             then newAlpha          -- beta cutoff
             else go cs newAlpha
    -- minimizer
    goMin [] curBeta = curBeta
    goMin (c:cs) curBeta =
      case dropPiece b c player of
        Nothing       -> goMin cs curBeta
        Just (nb, _)  ->
          let childScore = minimax nb (depth-1) alpha curBeta (opponent player) True
              newBeta    = min curBeta childScore
          in if newBeta <= alpha
             then newBeta           -- alpha cutoff
             else goMin cs newBeta

-- | Choose best column for player using minimax at given half-move depth.
bestMove :: Board -> Player -> Int -> Int
bestMove b p depth = fst $ maximumBy (comparing snd) scored
  where
    moves  = availableCols b
    scored = [ (c, score c) | c <- moves ]
    score c = case dropPiece b c p of
      Nothing      -> minBound
      Just (nb, _) -> minimax nb (depth-1) (-10000000) 10000000 (opponent p) False

-- ============================================================
-- Console display
-- ============================================================

showBoard :: Board -> String
showBoard b = unlines $
    [ "Board " ++ show (rows b) ++ "x" ++ show (cols b)
      ++ " K=" ++ show (winK b)
      ++ " Forbidden=" ++ show (forbidden b) ]
    ++ [ concatMap (showCell b r) [1..cols b] ++ "|"
       | r <- [1..rows b] ]
  where
    showCell brd r c
      | isForbidden brd r c = "|#"
      | otherwise = case cells brd !! (r-1) !! (c-1) of
          Nothing      -> "| "
          Just X       -> "|X"
          Just O       -> "|O"

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard

-- ============================================================
-- Example usage (run in GHCi or main)
-- ============================================================
-- > let b = newGame 5 6 4 []
-- > let Just (b1, _) = dropPiece b 3 X
-- > let c = bestMove b1 O 6
-- > let Just (b2, _) = dropPiece b1 c O
-- > printBoard b2
-- > winExists b2 X
-- ============================================================
