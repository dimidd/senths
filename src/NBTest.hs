module NBTest where
import NBTrain

endToEnd :: IO (Int, Int, Float)
endToEnd = tsvsToStats "../data/train.tsv" "../data/test.tsv"
