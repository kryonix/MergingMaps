-- Author: Denis Hirn
module MergingMaps where

import Data.List
import Data.List.Extra(groupSort)
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Ord
import Debug.Trace
import Control.Monad
import Data.Foldable
--------------------------------------------------------------------------------
-- Data types

compareScoreOffset :: (Ord a, Ord b) => (a,b) -> (a,b) -> Ordering
compareScoreOffset (a1, b1) (a2, b2) | cmpA == EQ = compare b1 b2
                                     | otherwise = cmpA
  where
    cmpA = compare a1 a2

compareTest (_, Just x@(_, _)) (_, Just y@(_, _)) = compare x y

compareTest2 :: (Score, (Tile, Tile, Offset)) -> (Score, (Tile, Tile, Offset)) -> Ordering
compareTest2 (s, (_,_,o)) (s2, (_,_,o2)) = compare (s, o) (s2, o2)


type Feature = Char

type RawTile = [String]

-- | Coordinates in a map tile
data Coord = C
    { cRow :: Int
    , cCol :: Int
    } deriving (Show, Eq, Ord)

-- | An offset transformation for tile coordinates
data Offset = O
    { oRow :: Int
    , oCol :: Int
    } deriving (Eq)

instance Show Offset where
    show (O r c) = show (r, c)

instance Ord Offset where
    o1 <= o2 = oRow o1 <= oRow o2 && oCol o1 <= oCol o2

-- | A map tile. Only store the major features.
data Tile = T
    { tCols     :: Int
    , tRows     :: Int
    , tFeatures :: M.Map Coord Feature
    } deriving (Show, Eq)

type Score = Int

type Map = [String]

-------------------------------------------------------------------------------
-- Converting raw tiles from and to internal tiles

-- | Convert a raw tile into our internal tree-based tile.
fromRawTile :: RawTile -> Tile
fromRawTile []      = error "tile is empty"
fromRawTile t@(r:_) = T cols rows (M.fromList features)
  where
    cols     = length r
    rows     = length t
    features = concat [ [ (C i j, f) | isMajorFeature f ]
                      | (row, i) <- zip t [0..]
                      , (f, j)   <- zip row [0..]
                      ]

isMajorFeature :: Feature -> Bool
isMajorFeature c = c `elem` ['A'..'Z']

-- | Convert the internal tile representation into a character map.
toMap :: Tile -> Map
toMap t = [ [ fromMaybe '-' (M.lookup c (tFeatures t))
            | col <- [0..tCols t - 1]
            , let c = C { cRow = row, cCol = col }
            ]
          | row <- [0..tRows t - 1]
          ]

addBorder :: Map -> Map
addBorder m = top ++ map (\ x -> '|':x++"|") m ++ top
  where
    cols = length (head m)
    top = ['+': replicate cols '-' ++"+"]

-- | Render the map into a string.
renderMap :: Map -> String
renderMap = intercalate "\n"

-- | Render multiple tiles as maps into a string.
showMaps :: [Tile] -> String
showMaps = intercalate "\n\n" . map (renderMap . addBorder . toMap)


-- | Convert the internal tile representation into a character map.
toMap' :: Tile -> Map
toMap' t = (show (tRows t) ++ " " ++ show (tCols t)):[ [ fromMaybe '-' (M.lookup c (tFeatures t))
            | col <- [0..tCols t - 1]
            , let c = C { cRow = row, cCol = col }
            ]
          | row <- [0..tRows t - 1]
          ]

-- | Render multiple tiles. Used for Data Generator (without border)
showMapsPlain :: [Tile] -> String
showMapsPlain = intercalate "\n" . map (renderMap . toMap')

-------------------------------------------------------------------------------
-- Algorithm

possibleOffsets :: Tile -> Tile -> [Offset]
possibleOffsets ta tb = res
   where
      ta_cols = tCols ta - 1
      ta_rows = tRows ta - 1
      tb_cols = tCols tb - 1
      tb_rows = tRows tb - 1

      res = [O x y
            | x <- [-(min ta_rows tb_rows)..(max ta_rows tb_rows)]
            , y <- [-(min ta_cols tb_cols)..(max ta_cols tb_cols)]
            ]

xsum :: Int -> Maybe Int -> Maybe Int
xsum _ Nothing = Nothing
xsum a (Just x) = Just (x+a)

computeScore :: Tile -> Tile -> Offset -> Maybe Score
computeScore ta tb (O row col) = do
    --Zu prüfenden Bereich generieren
    --Bereich auf Koordinaten Mappen
    let tfa = tFeatures ta
    let tfb = tFeatures tb
    foldM xsum 0 [check
                 |
                 x <- take (tRows tb-row) [row..],
                 y <- take (tCols tb-col) [col..],
                 let c1 = M.lookup (C x y) tfa,
                 isJust c1,
                 let c2 = M.lookup (C (x-row) (y-col)) tfb,
                 isJust c2,
                 let check = checkOffset (c1, c2),
                 check /= Just 0
                 ]

checkOffset :: (Maybe Feature, Maybe Feature) -> Maybe Int
checkOffset (Just x, Just y)   | x == y = Just 1
                               | otherwise = Nothing
checkOffset _ = Just 0

bestOffsets :: Tile -> Tile -> Maybe (Score, [Offset])
bestOffsets ta tb = res
   where
      scores = [(a, x)
               | x <- possibleOffsets ta tb    -- Calculate all offsets
               , let p = computeScore ta tb x  -- Compute score
               , isJust p && p /= Just 0       -- Filter invalid scores
               , let (Just a) = p]             -- Unpack just

      scoreCollected = maximumBy (comparing fst) $ groupSort scores
      res | null scores = Nothing
          | otherwise = Just scoreCollected

bestOffset :: Tile -> Tile -> Maybe (Score, Offset)
bestOffset ta tb = (\(x, y) -> (,) x (minimum y)) <$!> bestOffsets ta tb

merge :: Offset -> Tile -> Tile -> Tile
merge (O r c) ta@(T c1 r1 f1) tb@(T c2 r2 f2) | r >= 0 && c >= 0 = tNew
                                              | otherwise = merge (O (abs r) (abs c)) tb ta
   where
      nRows = max r1 (r2 + r)
      nCols = max c1 (c2 + c)
      tb_features' = M.mapKeys (\(C x y) -> (C (x+r) (y+c))) f2
      tNew = T nCols nRows (M.union f1 tb_features')

bestMatchForTile :: Tile -> [Tile] -> [(Score, (Tile, Tile, Offset))]
bestMatchForTile ta tn | null comb = []
                       | otherwise = res
   where
      --bestOffset auf t1 und jedes tn anwenden
      --Nothing ergebnisse filtern
      comb :: [(Tile, Maybe (Score, Offset))]
      comb = filter (isJust . snd) $ zip tn $ map (bestOffset ta) tn
      maxScore = (\(s, Just (t, o)) -> (s, t, o)) $ maximumBy compareTest comb
      res = [(\(t, s, o) -> (s, (ta, t, o))) maxScore]

mergeTiles :: [Tile] -> [Tile]
mergeTiles [] = []
mergeTiles [t] = [t]
mergeTiles ta | isNothing maxScore || null combMap = ta
              | otherwise = mergeTiles resMerge
   where
      -- Check all combinations left in list
      combinations = zip ta (drop 1 $ tails ta)
      -- Calculate best offset and score per combination
      combMap = concatMap (uncurry bestMatchForTile) combinations
      -- Choose element such that score is maximal and offset minimal
      maxScore = if null combMap then Nothing else Just $ maximumBy compareTest2 combMap
      resMerge = maybe [] (\(_, (tx, tb, o)) -> merge o tx tb : (ta \\ [tx, tb])) maxScore
