module Table where

import System.IO
import qualified Data.ByteString.Lazy as BL
import Text.CSV
import qualified Data.CSV.Table as T
--import Data.CSV.Table hiding (Table)
import Text.Read (readMaybe)
import Data.List (nub, intersperse)

newtype Frame = Frame { unFrame :: T.Table }

instance Show Frame where
  show (Frame (T.T _ cls bdy)) =
    let csv = [c | T.C c <- cls] : [xs | T.R xs <- bdy]
        printCSV records = unlines (printRecord `map` records)
          where printRecord = concat . intersperse "," . map printField
                printField f = f
                unlines = concat . intersperse "\n"
    in printCSV csv

type Row = [(String, Value)]
type Table = [Row]

customers :: Table
customers = [
   [("customerId", IntValue 1), ("customerName", StringValue "Alfred")]
  ,[("customerId", IntValue 2), ("customerName", StringValue "Ana")]
  ,[("customerId", IntValue 3), ("customerName", StringValue "Antonio")]]

orders :: Table
orders = [
   [("orderId", IntValue 10308), ("customerId", IntValue 2)]
  ,[("orderId", IntValue 10309), ("customerId", IntValue 1)]
  ,[("orderId", IntValue 10310), ("customerId", IntValue 3)]
  ,[("orderId", IntValue 10311), ("customerId", IntValue 2)]]

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

fromFrame :: Frame -> Table
fromFrame (Frame (T.T _ cols body)) =
  let columns = map (\(T.C x) -> x) cols
      rawRows = map (\(T.R x) -> x) body
      readRow = map readValue
      rows = map readRow rawRows
  in map (zip columns) rows

toFrame :: Table -> Frame
toFrame rowList = Frame (T.T dim' cols' body')
  where allTags = map (map fst) rowList
        cols' = case nub allTags of
          [tags] -> map T.C tags
          _ -> error "The Impossible Happened: Mismatching column names for different rows"
        dim' = length cols'
        allVals = map (map snd) rowList
        allRows = map (map unwrapValue) allVals
        body' = map T.R allRows

fromFile :: FilePath -> IO Frame
fromFile f = Frame <$> T.fromFile f

toFile :: FilePath -> Frame -> IO ()
toFile f = writeFile f . show

readTable :: FilePath -> IO (Table)
readTable filepath = do
  frame <- fromFile filepath
  return $ fromFrame frame

writeTable :: FilePath -> Table -> IO ()
writeTable filepath table = toFile filepath $ toFrame table
  
