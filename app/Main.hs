module Main where

import Data.List (delete)
import System.Environment (getArgs)
import System.FilePath (splitExtension)

-- List utilities
segment :: [a] -> [[a]]
segment [] = []
segment [x, y] = [[x, y]]
segment (x : y : rest) = [x, y] : segment rest

inAny :: (Eq a) => [[a]] -> a -> Bool
inAny lists e = any (e `elem`) lists

replace :: [a] -> Int -> a -> [a]
replace list index e = let (x, _ : ys) = splitAt index list in x ++ e : ys

-- Data & types and related functions
data Tile
  = Wall
  | Passage
  | Path
  | Start
  | Goal
  deriving (Eq)

instance Show Tile where
  show Wall = "##"
  show Passage = "  "
  show Path = "::"
  show Start = "[]"
  show Goal = "><"

parseTile :: String -> Tile
parseTile s
  | s == show Wall = Wall
  | s == show Passage = Passage
  | s == show Path = Path
  | s == show Start = Start
  | s == show Goal = Goal

newtype Maze
  = Maze [[Tile]]

instance Show Maze where
  show (Maze tiles) = unlines $ map (\row -> concatMap show $ tiles !! row) [0 .. length tiles - 1]

parseMaze :: String -> Maze
parseMaze mazeStr =
  Maze ([[parseTile t | t <- segment str] | str <- lines mazeStr])

at :: Maze -> Coords -> Tile
at (Maze tiles) (row, col) = tiles !! row !! col

getHeight :: Maze -> Int
getHeight (Maze tiles) = length tiles

getWidth :: Maze -> Int
getWidth (Maze tiles) = length $ head tiles

type Coords =
  (Int, Int)

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq)

data Node = Node
  { getCoords :: Coords,
    getF :: Int,
    getG :: Int,
    getFrom :: Node,
    getDir :: Direction
  }

instance Eq Node where
  node == other = getCoords node == getCoords other

instance Ord Node where
  (Node _ f1 _ _ _) `compare` (Node _ f2 _ _ _) = f1 `compare` f2

createNode :: Coords -> Node -> (Coords -> Int) -> Node
createNode coords from heuristic =
  Node
    { getCoords = coords,
      getG = g',
      getF = g' + h',
      getFrom = from,
      getDir = direction
    }
  where
    g' = getG from + 1
    h' = heuristic coords
    direction = getRequiredDirection (getCoords from) coords

-- A* algorithm 

getRequiredDirection :: Coords -> Coords -> Direction
getRequiredDirection (r1, c1) (r2, c2)
  | c1 == c2 && r1 - 1 == r2 = North
  | c1 == c2 && r1 + 1 == r2 = South
  | r1 == r2 && c1 - 1 == c2 = West
  | r1 == r2 && c1 + 1 == c2 = East

getInDirection :: Coords -> Direction -> Coords
getInDirection (row, col) d
  | d == North = (row - 1, col)
  | d == South = (row + 1, col)
  | d == West = (row, col - 1)
  | d == East = (row, col + 1)

getNeighbors :: Maze -> Node -> (Coords -> Int) -> [Node]
getNeighbors maze n heuristic =
  [ createNode (r1, c1) n heuristic | r1 <- [(row - 1) .. (row + 1)], 
                                      r1 >= 0, 
                                      r1 < getHeight maze,
                                      c1 <- [(col - 1) .. (col + 1)], 
                                      c1 >= 0,
                                      c1 < getWidth maze, 
                                      row - r1 == 0 || col - c1 == 0, 
                                      at maze (r1, c1) /= Wall
  ]
  where
    (row, col) = getCoords n

manhattanHeuristic :: Coords -> Coords -> Int
manhattanHeuristic (p1, p2) (q1, q2) = abs (p1 - q1) + abs (p2 - q2)

replaceTile :: Maze -> Coords -> Tile -> Maze
replaceTile (Maze tiles) (row, col) t = Maze (replace tiles row $ replace (tiles !! row) col t)

backtrace :: Maze -> Node -> [Direction]
backtrace maze node@(Node coords _ _ from direction) =
  if at maze coords == Start
    then []
    else backtrace maze from ++ [direction]

aStar :: Maze -> Coords -> (Coords -> Coords -> Int) -> [Node] -> [Node] -> Maybe [Direction]
aStar _ _ _ [] _ = Nothing
aStar maze goal heuristic openSet closedSet =
  let pivot = minimum openSet
      neighbors = filter (not . inAny [openSet, closedSet]) $ getNeighbors maze pivot (heuristic goal)
      openSet' = neighbors ++ delete pivot openSet
      closedSet' = pivot : closedSet
   in if getCoords pivot == goal
        then Just $ backtrace maze pivot
        else aStar maze goal heuristic openSet' closedSet'

carvePath :: Maze -> Coords -> [Direction] -> Maze
carvePath maze _ [] = maze
carvePath maze coords (d : path) = carvePath maze' coords' path
  where
    coords' = getInDirection coords d
    maze' =
      if at maze coords == Passage
        then replaceTile maze coords Path
        else maze

solve :: Maze -> Coords -> Coords -> Maybe Maze
solve maze start goal = case path of
  Nothing -> Nothing
  Just p -> Just $ carvePath maze start p
  where
    path = aStar maze goal manhattanHeuristic [startNode] []
    startNode =
      Node
        { getCoords = start,
          getF = manhattanHeuristic goal start,
          getG = 0,
          getFrom = startNode,
          getDir = North
        }

parseArgs :: [String] -> IO (String, Coords, Coords)
parseArgs args =
  if length args < 3
    then error $ "Arguments must be 3, provided " ++ show (length args) ++ "!"
    else do
      let start = read $ args !! 1 :: Coords
          goal = read $ args !! 2 :: Coords

      return (head args, start, goal)

main :: IO ()
main = do
  args <- getArgs
  (filename, start, goal) <- parseArgs args

  mazeStr <- readFile filename
  let m' = replaceTile (parseMaze mazeStr) start Start
      maze = replaceTile m' goal Goal
      solved = solve maze start goal

  putStrLn "Maze to solve:"
  print maze

  case solved of 
    Nothing -> putStrLn "Solution not found!"
    Just m -> do
      putStrLn "Solved!"
      print m

      let solutionPath = filepath ++ "_solved" ++ ext where (filepath, ext) = splitExtension filename

      writeFile solutionPath $ show m
      putStrLn $ "Solution saved at " ++ solutionPath