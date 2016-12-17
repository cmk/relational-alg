module Table where

import System.IO
import Data.Csv
import Data.Text

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

data Value = BoolValue Bool | IntValue Int | RealValue Double | StringValue Text

newtype ColName = ColName Text

type Row = [(ColName, Value)]
type Table = [Row]


--TODO: use utf8 to convert ByteString -> Text 

type Person = (Integer, BL.ByteString, BL.ByteString, Integer, Double, BL.ByteString)

personStats :: BL.ByteString -> Either String (V.Vector Person)
personStats = decode HasHeader

getVec :: IO ()
getVec  = do
  csvData <- BL.readFile "data/people.csv"
  let vec = personStats csvData
  print vec
