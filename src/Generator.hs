import System.Environment
import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, random, randomR, newStdGen)
import qualified Data.Map as M
import MergingMaps

myRand :: Int -> State StdGen Int
myRand x = do
    gen <- get
    let (r, nextGen) = randomR (0, x-1) gen
    put nextGen
    return r

randomFeature :: State StdGen Feature
randomFeature = do
    gen <- get
    let (r, nextGen) = randomR ('A', 'F') gen
    put nextGen
    return r

randomCoord :: Int -> Int -> State StdGen Coord
randomCoord x y = do
    x1 <- myRand x
    x2 <- myRand y
    return (C x1 x2)

randomTile :: Int -> Int -> State StdGen Tile
randomTile x y = do
    coords <- mapM (const $ randomCoord x y) $ replicate x ()
    features <- mapM (const randomFeature) $ replicate y ()
    let m = zip coords features
    return (T x y (M.fromList m))

randomDataSet :: Int -> Int -> Int -> State StdGen [Tile]
randomDataSet n x y = do
    tiles <- mapM (const (randomTile x y)) $ replicate n ()
    return tiles

main = do
    [numMaps, x, y] <- getArgs
    let n = read numMaps :: Int
    let x' = read x :: Int
    let y' = read y :: Int
    gen <- newStdGen
    let dataSet = evalState (randomDataSet n x' y') $ gen
    putStrLn $ show n
    putStrLn $ showMapsPlain dataSet
    putStrLn "0"