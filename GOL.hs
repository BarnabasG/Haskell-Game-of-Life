import Data.List ( intercalate, nub )

type Point                                              -- New type to represent a coordinate
    = (Int,Int)

glider :: [Point]                                       -- A common infinite pattern in
glider                                                  -- game of life
    =[(0,2), (1,3), (2,1), (2,2), (2,3)]

pretty :: [[String]] -> String                          -- Using Data.List.intercalate
pretty [] = []                                          -- This function inserts "\n" between
pretty (x:xs)                                           -- the elements of the list x,
    =  intercalate "\n" x ++ "\n\n" ++ pretty xs          -- and concatenates the result

visualisation :: Int -> Int -> [[Point]] -> [[String]]  -- Displays an n x m grid containing
visualisation _ _ [] = []                               -- #s for live cells and .s for dead
visualisation a b (xxs:rest)
    = concatenator (chunks a (findGrid a b xxs)) : visualisation a b rest

evolution :: [Point] -> [[Point]]                       -- Produces a potentially-infinite
                                                        -- sequence of generations of live cells 
evolution = iterate nextGeneration

checkPoints :: Int -> Int -> [Point] -> String
checkPoints a b [] = "."                                -- Cell is dead
checkPoints a b ((x, y):xxs)
    |   a == y && b == x    = "#"                       -- Cell is alive
    |   otherwise           = checkPoints a b xxs

chunks :: Int -> [a] -> [[a]]                           -- Split list of a (polymorphic)
chunks _ [] = []                                        -- into chunks of length n
chunks n xs =
    let (ys, zs) = splitAt n xs
    in ys : chunks n zs

concatenator :: [[String]] -> [String]                  -- Flatten nested list to single list
concatenator = map concat

findGrid :: Int -> Int -> [Point] -> [String]           -- Loop through each coordinate in grid
findGrid a b xxs                                      -- using 'do' and return life status
    = do
        c <- [0..a-1]
        d <- [0..b-1]
        return (checkPoints c d xxs)

neighbours :: Point -> [Point]                          -- Finds all neighbours of a given point
neighbours (x, y)
    = [(x+a, y+b) | a <- [-1..1], b <- [-1..1], (a,b) /= (0,0)]

liveNeighbours :: [Point] -> Point -> [Point]           -- Returns the living neighbours of
liveNeighbours [] _ = []                                -- the given point
liveNeighbours ((x,y):rest) (a, b)
    |   (x,y) `elem` neighbours(a, b)    = (x,y) : liveNeighbours rest (a, b)
    |   otherwise           = liveNeighbours rest (a, b)

coordFind :: [Point] -> [[Point]]                       -- Returns list of all the neighbours of
coordFind [] = []                                       -- all the points given 
coordFind ((x, y):xxs)
    =   neighbours (x, y) : coordFind xxs

allCoords :: [Point] -> [Point]                         -- Finds all the coordinates needed to assess
allCoords points                                        -- in a concatenated list
    = nub(concat(coordFind points))                     -- Data.List.nub removes duplicate points from list

applyRules :: [Point] -> [Point] -> [Point]             -- Applies the standard cell life rules
applyRules [] _ = []                                    -- of Conway's Game of life to the relevant
applyRules ((x, y):xxs) live                          -- cells for the next generation
    |   (x, y) `elem` live
    && (length(liveNeighbours live (x, y)) >= 2
    && length(liveNeighbours live (x, y)) <= 3)
    =   (x, y) : applyRules xxs live
    |   (x, y) `notElem` live
    && length(liveNeighbours live (x, y)) == 3
    =   (x, y) : applyRules xxs live
    |   otherwise   = applyRules xxs live

nextGeneration :: [Point] -> [Point]                    -- Applies the rules of game of life with
nextGeneration xxs                                      -- the relevant inputs
    = applyRules (allCoords xxs) xxs

    
main :: IO ()
main
    = putStrLn (pretty (take 8 (visualisation 5 5 (evolution glider))))
