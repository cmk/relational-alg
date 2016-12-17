import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
-- cassava
import Data.Csv

type Person = (Integer, BL.ByteString, BL.ByteString, Integer, Double, BL.ByteString)


fourth :: (a, b, c, d, e, f) -> d
fourth (_, _, _, d, _, _) = d

personStats :: BL.ByteString -> Either String (V.Vector Person)
personStats = decode HasHeader

getSum :: IO ()
getSum  = do
  csvData <- BL.readFile "../data/people.csv"
  let summed = fmap (V.foldr summer 0) (personStats csvData)
  putStrLn $ "Total age was: " ++ (show summed)
  where summer = (+) . fourth

-- getSum :: FilePath -> IO Integer
-- getSum path = do
--   csvData <- BL.readFile path  
--   return $ fmap (V.foldr summer 0) (personStats csvData)

-- printSum :: IO ()
-- printSum = do
--   summed <- getSum "../data/people.csv"
--   putStrLn $ "Total age was: " ++ (show summed)

-- main :: IO ()
-- main = hspec $ do
--   describe "Verify that summing works" $ do
--     it "equals zero" $ do
--       theSum <- getSum "../data/people.csv"
--       theSum `shouldBe` 458
