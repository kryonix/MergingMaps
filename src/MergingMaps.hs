-- Author: Denis Hirn
module MergingMaps where

import Data.List
import Data.List.Extra(groupSort)
import Data.Maybe
import qualified Data.Map as M
import Data.Ord
--------------------------------------------------------------------------------
-- Data types

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
toMap' t = (show (tCols t) ++ " " ++ show (tRows t)):[ [ fromMaybe '-' (M.lookup c (tFeatures t))
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
possibleOffsets ta tb = [O x y | x <- xInt, y <- yInt]
   where
      ta_cols = tCols ta - 1
      ta_rows = tRows ta - 1
      tb_cols = tCols tb - 1
      tb_rows = tRows tb - 1

      xInt = [-(min ta_rows tb_rows)..(max ta_rows tb_rows)]
      yInt = [-(min ta_cols tb_cols)..(max ta_cols tb_cols)]

computeScore :: Tile -> Tile -> Offset -> Maybe Score
computeScore ta tb (O row col) = do
    --Zu prüfenden Bereich generieren
    --Bereich auf Koordinaten Mappen
    let test = [C x y
               |
               x <- take (tRows tb-row) [row..],
               y <- take (tCols tb-col) [col..]
               ]

    test' <- mapM (\x -> checkOffset (M.lookup x (tFeatures ta),
                               M.lookup (C (cRow x-row) (cCol x-col)) (tFeatures tb))) test

    return (sum test')

checkOffset :: (Maybe Feature, Maybe Feature) -> Maybe Int
checkOffset (Just x, Just y)   | x == y = Just 1
                               | otherwise = Nothing
checkOffset _ = Just 0

bestOffsets :: Tile -> Tile -> Maybe (Score, [Offset])
bestOffsets ta tb = res
   where
      --Alle offsets berechnen
      offsets = possibleOffsets ta tb
      --Scores filtern
      scores  = filter (\(x,_) -> x /= Just 0) $
                  filter (isJust . fst) $
                     map (\x -> (,) (computeScore ta tb x) x) offsets

      scores' = map (\(Just x, y) -> (x, y)) scores
      scoreCollected = maximumBy (comparing fst) $ groupSort scores'
      res | null scores = Nothing
          | otherwise = Just scoreCollected

bestOffset :: Tile -> Tile -> Maybe (Score, Offset)
bestOffset ta tb = (\(x, y) -> (,) x (minimum y)) <$> bestOffsets ta tb

merge :: Offset -> Tile -> Tile -> Tile
merge (O r c) ta tb | r >= 0 && c >= 0 = tNew
                    | otherwise = merge (O (abs r) (abs c)) tb ta
   where
      nRows = max (tRows ta) (tRows tb + r)
      nCols = max (tCols ta) (tCols tb + c)
      tb_features' = M.mapKeys (\(C r1 c1) -> (C (r1+r) (c1+c))) (tFeatures tb)
      tNew = T nCols nRows (M.union (tFeatures ta) tb_features')

{-
Zwei kleine Helper, die die Auswahl der Tiles in bestMatchForTile stark vereinfacht.
-}
compareScoreOffset :: (Ord a, Ord b) => (a,b) -> (a,b) -> Ordering
compareScoreOffset (a1, b1) (a2, b2) | cmpA == EQ = compare b1 b2
                                     | otherwise = cmpA
  where
    cmpA = compare a1 a2

compareTest (_, Just x@(_, _)) (_, Just y@(_, _)) = compare x y--compareScoreOffset x y

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

compareTest2 :: (Score, (Tile, Tile, Offset)) -> (Score, (Tile, Tile, Offset)) -> Ordering
compareTest2 (s, (_,_,o)) (s2, (_,_,o2)) = compare (s, o) (s2, o2)--compareScoreOffset (s, o) (s2, o2)

mergeTiles :: [Tile] -> [Tile]
mergeTiles [] = []
mergeTiles [t] = [t]
mergeTiles ta | isNothing maxScore = ta
              | null combMap  = ta
              | otherwise = mergeTiles resMerge
   where
      combinations = zip ta (drop 1 $ tails ta)  --All combinations to check
      combMap = concatMap (uncurry bestMatchForTile) combinations  --Compute score
      --Wählt das best mögliche Element aus mittels
      --comparator compareTest2
      maxScore = if null combMap then Nothing else Just $ maximumBy compareTest2 combMap
      resMerge = maybe [] (\(_, (tx, tb, o)) -> merge o tx tb : (ta \\ [tx, tb])) maxScore
