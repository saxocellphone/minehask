{-# LANGUAGE LambdaCase #-}
module Game where

import Data.List
import Data.Maybe
import Data.List.Split
import Control.Monad.Random
import Control.Monad.IO.Class
import Control.Monad.Reader

data Square = Square Bool SquareStatus | NumberedSquare Int
data SquareStatus = Untouched | LostMine | Flagged
data Pos = Pos Int Int deriving (Eq)
data Env = Env {
  getInitPos :: Pos,
  getWidth :: Int,
  getHeight :: Int,
  getNumMines :: Int
}

type Board = [[Square]]
type Game a = ReaderT Env IO a

instance Show Square where
  show (Square _ Untouched) = "."
  show (Square _ LostMine) = "x"
  show (Square _ Flagged) = "f"
  show (NumberedSquare num) = if num > 0 then show num else " "

infixl 6 |+|
Pos a b |+| Pos c d = Pos (a+c) (b+d)

---------------
-- Game Control
---------------
play :: Int -> Int -> Int -> IO ()
play w h numMines = do
  initPos <- readInput
  board <- runReaderT createRandomBoard $ Env initPos w h numMines
  play' board initPos

play' :: Board -> Pos -> IO ()
play' board initPos = case expand board [initPos] of
    Just nextBoard -> do 
      printBoard nextBoard
      if checkWin nextBoard then 
        print "You won!"
      else do
        nextPos <- readInput
        play' nextBoard nextPos
    Nothing -> do
      print "You Lost :("
      printBoard $ markBoard board

readInput :: IO Pos
readInput = do
  pos <- do
    putStrLn "Make a move: (Format Int Int)"
    getLine
  let [initX, initY] = splitOn " " pos
  return $ Pos (read initX - 1) (read initY - 1)

--------------
--Impure Stuff
--------------
createRandomBoard :: Game Board
createRandomBoard = do
  width <- asks getWidth
  height <- asks getHeight
  randomLayout <- randMines
  let field = take height $ chunksOf width $ genField randomLayout $ width * height
  lift $ return field

randMines :: Game [Int]
randMines = do
  width <- asks getWidth
  height <- asks getHeight
  pos <- asks getInitPos
  n <- asks getNumMines
  randomSample n $ delete (posToIndex pos width height) [0..width*height-1]

randomSample :: Int -> [a] -> Game [a]
randomSample 0 list = pure []
randomSample k list = do
  i <- getRandomR (0, length list - 1)
  let (a, xs) = splitAt i list
  l <- if not (null xs) then randomSample (k-1) (a ++ tail xs) else lift $ return []
  pure $ if not (null xs) then head xs : l else l

----------------
--Pure Functions
----------------
genField :: [Int] -> Int -> [Square]
genField mines = genField' (sort mines) 0
  where
  genField' [] index size = replicate (size - index) (Square False Untouched)
  genField' mines@(x:xs) index size
    | x == index = Square True Untouched : genField' xs (index+1) size
    | otherwise = Square False Untouched : genField' mines (index+1) size

getSquare :: Board -> Pos -> Maybe Square
getSquare b (Pos x y)
  | x >= length b || x < 0 = Nothing
  | y >= length (head b) || y < 0 = Nothing
  | otherwise = Just (b !! x !! y)

getNearMines :: Board -> Pos -> Int
getNearMines b pos =
  let 
    d = [-1, 0, 1]
    dirs = (|+|) <$> [Pos a b| a <- d, b <- d] <*> [pos]
  in
    foldl (\acc p -> case getSquare b p of
                       Just (Square True _) -> acc + 1
                       _ -> acc) 0 dirs
  
getExpansions :: Board -> Pos -> [Pos]
getExpansions b pos =
  case getSquare b pos of
    Nothing -> []
    Just (Square True _) -> []
    Just _ -> expansions
  where
    isZero = getNearMines b pos == 0
    ds = if isZero then
            [Pos a b | a <- [-1, 0, 1], b <- [-1, 0, 1]]
          else
            [Pos a b | (a,b) <- [(-1, 0), (0, -1), (1, 0), (0, 1), (0, 0)]]
    dirs = (|+|) <$> ds <*> [pos]
    bounded_dirs = filter (\(Pos x y) -> x >= 0 && y >= 0) dirs
    filtered_dirs = if isZero then 
                      bounded_dirs
                    else
                      filter (\n -> n == pos || getNearMines b n == 0) bounded_dirs
    expansions = foldl (\acc p -> case getSquare b p of
                        Just s@(Square False Untouched) -> p : acc
                        _ -> acc) [] filtered_dirs

expand :: Board -> [Pos] -> Maybe Board
expand b p = do
  let expansions = concat $ mapMaybe (expand' b) p
  let newboard = foldr (\(ri, row) r ->
                  foldr (\(ci, s) c ->
                    if Pos ri ci `elem` expansions then
                      NumberedSquare (getNearMines b $ Pos ri ci) : c
                    else 
                      s : c
                  ) [] (zip [0..] row) : r
                ) [] (zip [0..] b)
  let removeCur = filter (`notElem` p) expansions
  if not $ lost b p then
    if null removeCur then 
      return newboard 
    else 
      expand newboard removeCur
  else
    Nothing
  where
    expand' :: Board -> Pos -> Maybe [Pos]
    expand' b' p' =
      case getSquare b' p' of
        Nothing -> Nothing
        Just (Square True _ ) -> Nothing
        _ -> Just (getExpansions b' p')

    lost :: Board -> [Pos] -> Bool
    lost _ [] = False
    lost b' (x:xs) =
      case getSquare b' x of
        Just (Square True _) -> True
        _ -> lost b' xs

checkWin :: Board -> Bool
checkWin b = 
  all (==True) $ 
  fmap (all (==True) . 
    fmap (\case
            Square False Untouched -> False
            _ -> True)) b

-----------
--Utilities
-----------
indexToPos :: Int -> Int -> Int -> Pos
indexToPos index w h = Pos (mod index w) (index `div` w)

posToIndex :: Pos -> Int -> Int -> Int
posToIndex (Pos x y) w h = y * w + x

printBoard :: Board -> IO ()
printBoard b = do
  let width = length $ head b
  putStrLn $ replicate (width * 2) '-'
  mapM_ (putStrLn . unwords . fmap show) b
  putStrLn $ replicate (length b * 2) '-'

markBoard :: Board -> Board
markBoard b =
  foldr (\(ri, row) r ->
    foldr (\(ci, s) c ->
      case s of
        Square False _ -> NumberedSquare (getNearMines b $ Pos ri ci) : c
        Square True _ -> Square True LostMine : c
        _ -> s : c
    ) [] (zip [0..] row) : r
  ) [] (zip [0..] b)

parseBoard :: [Char] -> Board
parseBoard text = 
  fmap charToSquare <$> chunksOf 5 text
  where 
    charToSquare c
      | c == '*' = Square True Untouched
      | otherwise = Square False Untouched
