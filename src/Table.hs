module Table where

import System.IO
import Data.Csv
import Data.Text


data Value = BoolValue Bool | IntValue Int | RealValue Double | StringValue Text

newtype Name = Name Text

type Row = [(Name, Value)]
type Table = [Row]

inferType :: String -> Value
inferType "True" = True
inferType "False" = False
--etc

--TODO: use utf8 to convert ByteString -> Text 
