module Table where

import System.IO
import qualified Data.ByteString.Lazy as BL
import Text.CSV
import qualified Data.CSV.Table as T
import Data.CSV.Table hiding (Table)
import Text.Read (readMaybe)
import Data.List (nub, intersperse)

newtype Table = Table { unTable :: T.Table }

instance Show Table where
  show (Table (T _ cls bdy)) =
    let csv = [c | C c <- cls] : [xs | R xs <- bdy]
        printCSV records = unlines (printRecord `map` records)
          where printRecord = concat . intersperse "," . map printField
                printField f = f
                unlines = concat . intersperse "\n"
    in printCSV csv

data Value
  = BoolValue Bool
  | IntValue Int
  | RealValue Double
  | StringValue String
  deriving (Read, Show, Eq, Ord)

getInt :: Value -> Int
getInt (IntValue i) = i

getBool :: Value -> Bool
getBool (BoolValue b) = b

getReal :: Value -> Double
getReal (RealValue r) = r

getString :: Value -> String
getString (StringValue s) = s

readValue :: String -> Value
readValue str = let
  value = boolValue
  boolValue = case readMaybe str of
    Nothing -> intValue
    Just b -> BoolValue b
  intValue = case readMaybe str of
    Nothing -> realValue
    Just i -> IntValue i
  realValue = case readMaybe str of
    Nothing -> StringValue str
    Just d -> RealValue d
  in value

unwrapValue :: Value -> String
unwrapValue val = case val of
  BoolValue b -> show b
  IntValue i -> show i
  RealValue r -> show r
  StringValue s -> s -- printCSV [[s]] -- properly handle escape sequences

fromTable :: Table -> [[(String, Value)]]
fromTable (Table (T _ cols body)) =
  let columns = map (\(C x) -> x) cols
      rawRows = map (\(R x) -> x) body
      readRow = map readValue
      rows = map readRow rawRows
  in map (zip columns) rows

toTable :: [[(String, Value)]] -> Table
toTable rowList = Table (T dim' cols' body')
  where allTags = map (map fst) rowList
        cols' = case nub allTags of
          [tags] -> map C tags
          _ -> error "The Impossible Happened: Mismatching column names for different rows"
        dim' = length cols'
        allVals = map (map snd) rowList
        allRows = map (map unwrapValue) allVals
        body' = map R allRows

fromFile :: FilePath -> IO Table
fromFile f = Table <$> T.fromFile f

toFile :: FilePath -> Table -> IO ()
toFile f = writeFile f . show
