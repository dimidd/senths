module Main where
import NBTrain (tsvToStats, tsvToModel)
import NB (testUtterance, NBModel, trainUtterance)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering), isEOF)
import Data.Char (toLower)

endToEnd :: IO (Int, Int, Float)
endToEnd = tsvToStats "../data/train.tsv" "../data/test.tsv"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    model <- tsvToModel "../data/train.tsv"
    loop model

loop :: NBModel -> IO ()
loop model = do
    putStrLn "Enter a tech/biz headline to classify. Empty string to exit"
    putStr "> "
    done <- isEOF
    if done then
        putStrLn "Bye."
    else
        do
            sent <- getLine
            case sent of
              "" -> putStrLn "Bye."
              _ -> processSentence sent model

processSentence :: String -> NBModel -> IO ()
processSentence sent model = do
                    let klass = testUtterance model sent
                    printClass sent $ case klass of
                      "t" ->  "Technology"
                      _ ->    "Business"
                    answer <- checkAnswer
                    if answer then
                        do
                            putStrLn "Great!"
                            loop model
                    else
                        do
                            putStrLn "OK, I stand corrected."
                            let correct = reverseClass klass
                            let newModel = trainUtterance model sent correct
                            loop newModel

printClass :: String -> String -> IO ()
printClass sent c = putStrLn $ " I believe '" ++ sent ++ "' belongs to: " ++ c

checkAnswer :: IO Bool
checkAnswer = do
    putStrLn "Was this classification correct? Y(es)/N(o)"
    putStr "> "
    answer <- getLine
    case answer of
        "" -> checkAnswer
        _ -> do
            let c = toLower $ head answer
            case c of
                'y' -> return True
                'n' -> return False
                _ -> checkAnswer

reverseClass :: String -> String
reverseClass c = case c of
                    "t" -> "b"
                    _ -> "t"
