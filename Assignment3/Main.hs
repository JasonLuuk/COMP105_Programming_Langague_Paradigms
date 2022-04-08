module Main (get_maze, print_maze, is_wall, place_player, move, can_move, game_loop, get_path, main) where 

import System.Environment
import Data.List (delete, findIndex)
import Data.Maybe


maze_path = "E:\\maze2.txt"
--m <- get_maze maze_path

-- Useful code from Lecture 25
-- You may use this freely in your solutions

get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x 

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set maze x y char = 
    let
        line = maze !! y
        new_line = modify_list line x char
        new_maze = modify_list maze y new_line
    in
        new_maze

---- Part A

-- Question 1


get_maze :: String -> IO [String]
get_maze x = do
  file <-  readFile x
  return (lines file)

-- Question 2


print_maze :: [String] -> IO ()
print_maze x = putStrLn (unlines x)


-- Question 3

is_wall :: [String] -> (Int, Int) -> Bool
is_wall x (y,z) =
  if get x y z == '#' then True
  else False


-- Question 4

place_player :: [String] -> (Int, Int) -> [String]
place_player x (y,z) =  set x y z '@'


---- Part B

-- Question 5

move :: (Int, Int) -> Char -> (Int, Int)
move (x,y) z =
  if z == 'w' then (x,y-1)
  else if z == 's' then (x,y+1)
  else if z == 'a' then (x-1,y)
  else if z == 'd' then (x+1,y)
  else (x,y)


-- Question 6

can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move m (x,y) z =
  if (is_wall m (move (x,y) z)) then False
  else True

-- Question 7

game_loop :: [String] -> (Int, Int) -> IO ()
game_loop m (x,y) =
  do
    print_maze (place_player m (x,y))
    c <- getLine
    let first = head c
    if can_move m (x,y) first then game_loop m (move(x,y) first)
    else game_loop m (x,y)

---- Part C
-- Question 8

data Move = W | S | A | D deriving (Eq)

instance Show Move where
  show W = "w"
  show S = "s"
  show A = "a"
  show D = "d"

data Tile = Tile { getLoc :: Pos, getA :: Int, getB :: Int, getC :: Int, getF :: Tile, getMove :: Move } deriving (Show)

instance Eq Tile where
  a == b = getLoc a == getLoc b

type Map = [[Bool]]
type Pos = (Int, Int)

get_move_direction (x1, y1) (x2, y2)
  | x1 == x2     && y1 - 1 == y2 = W
  | x1 == x2     && y1 + 1 == y2 = S
  | x1 - 1 == x2 && y1 == y2     = A
  | x1 + 1 == x2 && y1 == y2     = D
  | otherwise                    = error "invalid move"

tile h from pos = Tile { getLoc = pos
                       , getB = g'
                       , getC = h'
                       , getA = g' + h'
                       , getF = from
                       , getMove = move
                       }
  where g' = getB from + 1
        h' = h pos
        move = get_move_direction (getLoc from) pos


to_move W (Tile (x, y) _ _ _ _ _) = (x    , y - 1)
to_move S (Tile (x, y) _ _ _ _ _) = (x    , y + 1)
to_move A (Tile (x, y) _ _ _ _ _) = (x - 1, y    )
to_move D (Tile (x, y) _ _ _ _ _) = (x + 1, y    )


isInMap m (x, y) = let r = length m
                       c = length $ head m
                   in  all id [ x >= 0, y >= 0, x < c, y < r ]

judge_blocked m (x, y) = m !! y !! x

judge_valid_loc m x = isInMap m x && judge_blocked m x

get_neighbors m h t = map (tile h t) $
                     filter (judge_valid_loc m) $
                     map (flip to_move t) $
                     [W, S, A, D]

manhattan (a, b) (c, d) = abs $ (a - c) + (b - d)

min_by y = foldl1 (\m x -> if y x < y m then x else m)

judge_in ass b = any id $ map (b`elem`) ass

find_small = min_by getA

back t@(Tile _ _ _ _ f move) = if f == t then []
                                    else back f ++ [move]

astar _ _ _ [] _ = Nothing
astar map end h open closed = let pivot = find_small open
                                  neighbors = filter (not . judge_in [open, closed]) $ get_neighbors map (h end) pivot
                                  open' = neighbors ++ delete pivot open
                                  closed' = pivot : closed
                              in  if (getLoc pivot) == end
                                    then Just $ back pivot
                                    else astar map end h open' closed'


solve m x y = astar m y manhattan [startTile] []
  where startTile = Tile { getLoc = x
                         , getA = manhattan y x
                         , getB = 0
                         , getC = manhattan y x
                         , getF = startTile
                         , getMove = W
                         }

deal_base [] = []
deal_base (x:xs) =
  if x =='#' then False:deal_base xs
  else True:deal_base xs

solve_final m x y = solve (map deal_base m) x y

unjust (Just x) =map show x

move_all (x,y) [] = []
move_all (x,y) (a:as) = move (x,y) (head a) : move_all (move (x,y) (head a)) as

move_all_add x y = x : move_all x y

final m x y = move_all_add x (unjust (solve_final m x y))

get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path m x y = move_all_add x (unjust (solve_final m x y))

-- Question 9

print_path m [] = print_maze m
print_path m (x:xs) = print_path (set m (fst x) (snd x) '.') xs 

main :: IO ()
main = do
  args <- getArgs
  m <- get_maze (head args)
  print_path m (get_path m (1,1) ((length (head m) - 2), (length m - 2)))
