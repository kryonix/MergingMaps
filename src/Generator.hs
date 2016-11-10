import System.Environment
import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, random, randomR, newStdGen)
import qualified Data.Map as M
import MergingMaps

myRand :: State StdGen Int
myRand = do
    gen <- get
    let (r, nextGen) = randomR (0, 5) gen
    put nextGen
    return r

randomFeature :: State StdGen Feature
randomFeature = do
    gen <- get
    let (r, nextGen) = randomR ('A', 'F') gen
    put nextGen
    return r

randomCoord :: State StdGen Coord
randomCoord = do
    x1 <- myRand
    x2 <- myRand
    return (C x1 x2)

randomTile :: State StdGen Tile
randomTile = do
    coords <- mapM (const randomCoord) $ replicate 5 ()
    features <- mapM (const randomFeature) $ replicate 5 ()
    let x = zip coords features
    return (T 5 5 (M.fromList x))

randomDataSet :: Int -> State StdGen [Tile]
randomDataSet n = do
    tiles <- mapM (const randomTile) $ replicate n ()
    return tiles

main = do
    --let triple = unGen randomTriple (mkStdGen 1) 1
    --let triple = randomTriple 
    --print triple
    --let values = mapM (const myRand) $ repeat ()
    --let triple = evalState values $ mkStdGen 1
    --print triple
    --gen <- newStdGen
    --let rndCoord = evalState randomCoord $ gen --mkStdGen 1
    --print rndCoord
    [numMaps] <- getArgs
    let n = read numMaps :: Int
    gen <- newStdGen
    let dataSet = evalState (randomDataSet n) $ gen
    putStrLn $ show n
    putStrLn $ showMapsPlain dataSet
    putStrLn "0"
    --let tile = evalState randomTile $ gen
    --putStrLn $ showMapsPlain [tile]