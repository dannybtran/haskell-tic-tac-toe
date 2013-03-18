import System.IO(hSetBuffering,stdin,BufferMode(NoBuffering))
import Data.Bits((.&.),setBit)
import Data.List(mapAccumL)

type Board  = [Int]
type Turn   =  Int
type Player =  Int
type Spot   =  Int

emptyBoard :: Board
emptyBoard = replicate 9 0

symbols :: Int -> [String]
symbols n = [(show n),"\ESC[32mX\ESC[0m","\ESC[31mO\ESC[0m"]

rotateListR :: [Int] -> Int -> [Int]
rotateListR [] _ = []
rotateListR l 0 = l
rotateListR l n = rotateListR ((last l):(init l)) (n-1)

place :: Board -> Player -> Spot -> Board
place board player spot = do
  zipWith (+) (rotateListR [player,0,0,0,0,0,0,0,0] spot) board

is_winner :: Board -> Player -> Bool
is_winner board player = do
  let a = ((listToBits (normalizeList board player) 0) :: Int)
  fst $ mapAccumL (\acc x -> if x == (x .&. a) then (True,x) else (acc,x)) False [7,56,73,84,146,273,292,448]

-- make list 1's and 0's depending on player
normalizeList :: Board -> Player -> [Int]
normalizeList board player = map (\x -> if x == player then 1 else 0) board

-- take a list of 1/0s and set bits of an int
listToBits :: [Int] -> Int -> Int
listToBits [] n = n
listToBits (x:xs) acc = do
  if x == 1 then
    listToBits (xs) (setBit acc (length xs))
  else
    listToBits (xs) acc

drawBoard :: Board -> Int -> IO ()
drawBoard board square
  | square == 9 = do
      return ()
  | (square+1) `mod` 3 == 0 = do
      putStr $ (symbols (square+1)) !! (board !! square)
      putStr "\n"
      drawBoard board (square+1)
  | otherwise = do
      putStr $ (symbols (square+1)) !! (board !! square)
      putStr " | "
      drawBoard board (square+1)

draw :: Board -> IO ()
draw board = do
  putStrLn "---------"
  drawBoard board 0
  putStrLn "---------"

engine :: Board -> Turn -> IO ()
engine board 9 = do
  draw board
  return ()
engine board turn = do
  let player = (turn`mod`2)+1
  draw board
  if player == 1 then do
    putStrLn $ "\ESC[32mPlayer " ++ (show player) ++ ", enter a move (1-9):\ESC[0m"
  else do
    putStrLn $ "\ESC[31mPlayer " ++ (show player) ++ ", enter a move (1-9):\ESC[0m"

  str <- getLine
  let move = (read str)-1
  if move > 8 || move < 0 || (board !! move) /= 0 then do
    putStrLn "Invalid move."
    engine board turn
  else do
    let newBoard = (place board player move)
    if is_winner newBoard player then do
      draw newBoard
      if player == 1 then
        putStrLn $ "\ESC[32mPlayer " ++ (show player) ++ " wins!\ESC[0m"
      else
        putStrLn $ "\ESC[31mPlayer " ++ (show player) ++ " wins!\ESC[0m"
    else
      engine newBoard (turn+1)

main = do
  hSetBuffering stdin NoBuffering
  engine emptyBoard 0