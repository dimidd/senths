module Main where
import NBTrain (tsvToStats, tsvToModel)
import NB (testUtterance, Model)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering) )

endToEnd :: IO (Int, Int, Float)
endToEnd = tsvToStats "../data/train.tsv" "../data/test.tsv"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    model <- tsvToModel "../data/train.tsv"
    loop model

loop :: Model -> IO ()
loop model = do
    putStrLn "Enter a tech/biz headline to classify. Empty string or Cntrl-C to exit"
    putStr "> "
    sent <- getLine
    case sent of
      "" -> putStrLn "Bye."
      _ -> do
            let klass = testUtterance model sent
            putStrLn $ case klass of
              "t" -> "tech"
              _ ->  "biz"
            loop model
