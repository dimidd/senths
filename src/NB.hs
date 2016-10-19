module NB where
import qualified Data.Map.Lazy as M
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Char (isAlpha, toLower, isSpace)
import Data.Monoid ((<>))

import StopWord

type Count = Int

-- A different spelling to prevent collision with keywords or prelude
type Werd = String
type Klass = String

type Utterance = String
type WerdCount = M.Map Werd Count
type KlassCount = M.Map Klass Count
type KlassWerds = M.Map Klass WerdCount

data Model = Model WerdCount KlassCount KlassWerds
    deriving Show

idModel :: Model
idModel = Model M.empty M.empty M.empty

-- create an empty model with one class
idModelWithCat :: Klass -> Model
idModelWithCat c = Model M.empty (M.fromList [(c, 1)]) M.empty

combineModels :: Model -> Model -> Model
combineModels (Model wc1 cc1 cw1) (Model wc2 cc2 cw2) =
    Model wc3 cc3 cw3
        where
            wc3 = M.union wc1 wc2
            cc3 = M.union cc1 cc2
            cw3 = M.union cw1 cw2

instance Monoid Model where
    mempty = idModel
    mappend = combineModels

trainUtterance :: Model -> Klass -> Utterance -> Model
trainUtterance m c ut = foldr (trainWerd c) m $ werds ut

-- We combine with idCat to insert the class just once
trainWerd :: Klass -> Werd -> Model -> Model
trainWerd c w (Model wc cc cw) = (Model wc' cc cw') <> idCat
    where
        wc' = upsertString wc w
        cw' = upsertMapString cw c w
        idCat = idModelWithCat c 

-- TODO: support N-grams
unigrams :: Utterance -> [Werd]
unigrams = words

-- normalized words (lower case alphabetic, without stop words)
werds :: Utterance -> [Werd]
werds = (filter isntStopWord) . unigrams . lowerChars
    where
        lowerChars = (filter isAlphaSpace) . (map toLower)
                                                
isAlphaSpace :: Char -> Bool
isAlphaSpace c = (isSpace c) || (isAlpha c)

-- If a word or a klass already exists, bump its count,
-- otherwise insert with a count of 1
upsertString :: M.Map String Count -> String -> M.Map String Count
upsertString m s = case M.lookup s m of
                     Nothing  ->  M.insert s 1 m
                     Just n   ->  M.insert s (n + 1) m

upsertMapString :: KlassWerds -> Klass -> Werd -> KlassWerds
upsertMapString cw c w = case M.lookup c cw of
                           Nothing  ->  M.insert c (M.singleton w 1)    cw
                           Just wc  ->  M.insert c (upsertString wc w)  cw

testUtterance :: Model -> Utterance -> Klass
testUtterance m u = fst $ maximumBy (comparing snd) $ catUtProbs m u

-- Return a list of classes, and the probability this utterance belongs to it.
-- For each klass, we take the probability of any utternace belonging to it,
-- and multiply it by the conditional probability of seeing such an utterance
-- given a klass.
-- Mathematical explanation:
-- We want to compute the conditional probability Pr(cat|ut),
-- i.e. the probability of belonging to a klass given an utterance.
-- According to Bayes' theorem:
-- Pr(cat|ut) = Pr(ut|cat) * Pr(cat)
--                    ------
--                    Pr(ut)
-- However, Pr(ut) doesn't depend on the klass.
-- Thus it's equal for all classes, and can be ommited.
-- See https://en.wikipedia.org/wiki/Naive_Bayes_classifier#Probabilistic_model
catUtProbs :: Model -> Utterance -> [(Klass, Double)]
catUtProbs (Model _ cc cw) ut = zipWith multProbs catUts cats
   where
       cats = catProbs cc
       catUts = utGivenCatProbs cw ut
       multProbs = (\(cat1, pr1) (_, pr2) -> (cat1, pr1 * pr2))

-- Return a list of classes and probabilites ( Pr(ut|cat) )
utGivenCatProbs :: KlassWerds -> Utterance -> [(Klass, Double)]
utGivenCatProbs cw ut = map (utProb ut) $ M.assocs cw

-- Calculate Pr(ut|cat)
-- We treat each word in the utterance as a feature and assume they're independent
-- i.e. seeing one word doesn't affect the probability of seeing another 
-- (this is the naive part). Thus, we multiply the probabilities of the words.
utProb :: Utterance -> (Klass, WerdCount) -> (Klass, Double)
utProb ut (cat, wc) = (cat, probRedux)
    where
        werdProbs = map (werdProb wc) (werds ut)
        probRedux = foldr (*) 1 werdProbs

-- Calculate the probability of a single word. To handle OOV (out of vocabulary)
-- words, i.e. words that apperar in the test set but not in the training set,
-- we add 1 to the number of appearnaces of the word in the klass.
werdProb :: WerdCount -> Werd -> Double
werdProb wc w = 
    let count = case M.lookup w wc of
                    Nothing -> 0
                    Just n -> n                
    in
        fromIntegral (count + 1) / (fromIntegral $ M.size wc)

-- Return a list of classes and their ratios (probabilities)
catProbs :: KlassCount -> [(Klass, Double)]
catProbs cc = map (countToRatio total) catCounts
   where
       catCounts = M.assocs cc :: [(Klass, Count)]
       total = fromIntegral $ M.size cc

countToRatio :: Double -> (Klass, Count) -> (Klass, Double)
countToRatio total (cat, count) = (cat, (fromIntegral count) / total)
