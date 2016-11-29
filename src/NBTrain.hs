module NBTrain where
import NB (trainUtterance, testUtterance, idModel, NBModel)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

trainLine :: String -> NBModel -> NBModel
trainLine l m = trainUtterance m cat ut
    where
        (cat, ut) = parseLine l

-- specifically for the tsv format, where the last field is a single char,
-- specifying the category
parseLine :: String -> (String, String)
parseLine l = (cat, ut)
    where
        reversedLine = reverse l
        cat = [head reversedLine]
        ut = reverse $ drop 2 reversedLine

trainFile :: NBModel -> FilePath -> IO NBModel
trainFile m f = do
    tsvLines <- readLines f
    return $ foldr trainLine m tsvLines

tsvToModel :: FilePath -> IO NBModel
tsvToModel = trainFile idModel

testLineIO :: IO NBModel -> String -> IO Bool
testLineIO im l = do
    m <- im
    return $ testLine m l

testLine :: NBModel -> String -> Bool
testLine m l = cat == testUtterance m ut
   where
        (cat, ut) = parseLine l

testFileIO :: IO NBModel -> FilePath -> IO [Bool]
testFileIO im f = do
    m <- im
    tsvLines <- readLines f
    return $ map (testLine m) tsvLines

-- Return a tuple of (#correct classifications, #classifications, %accuracy)
testStatsIO :: IO [Bool] -> IO (Int, Int, Float)
testStatsIO tests = do
    bools <- tests
    return $ testStats bools

testStats :: [Bool] -> (Int, Int, Float)
testStats bools = (correct, total, accuracy)
    where
        correct = (length . filter id) bools
        total = length bools
        accuracy = fromIntegral correct / fromIntegral total

tsvToStats :: FilePath -> FilePath -> IO (Int, Int, Float)
tsvToStats train test = do
    let im = tsvToModel train
    bools <- testFileIO im test
    return $ testStats bools

verifyMonoid :: IO ()
verifyMonoid = quickBatch (monoid idModel)
