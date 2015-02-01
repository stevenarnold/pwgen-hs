module Main where

import Data.Random
import Data.Char
import Data.List
import Data.Time.Clock.POSIX
import System.Environment
import System.Random

data CharLocation  = CharLocation Int Char
                     deriving (Show)
type CharLocations = [CharLocation]
data UserConfig = UserConfig { 
                                ucPwordLength :: Int,
                                ucNumCapitals :: Int,
                                ucNumNumbers :: Int, 
                                ucNumSpecials :: Int,
                                ucSpecialsToUse :: String
                             } deriving (Show)
userConfigDefault :: UserConfig
userConfigDefault = UserConfig { 
                      ucPwordLength = 24, 
                      ucNumCapitals = 3, 
                      ucNumNumbers = 1, 
                      ucNumSpecials = 1,
                      ucSpecialsToUse = "._-"
                    }

letterCharClass, numberCharClass :: Char -> Bool
letterCharClass = Data.Char.isUpper
numberCharClass = Data.Char.isDigit
stdAlternateCharset, capitalCharset, numericCharset :: [Char]
stdAlternateCharset = ['a' .. 'z']
-- These probably should be predicates
capitalCharset = ['A' .. 'Z']
numericCharset = ['0' .. '9']
specialCharClass, specialNotCharClass :: [Char] -> (Char -> Bool)
specialCharClass = mkCharsetPredicate True
specialNotCharClass = mkCharsetPredicate False

mkCharsetPredicate :: Bool -> [Char] -> (Char -> Bool)
mkCharsetPredicate positive charset =
  charsetPredicate 
  where charsetPredicate char = if positive then
                                  elem char charset
                                else
                                  not $ elem char charset

isCR :: Char -> Bool
isCR char = '\r' == char

isNotSpace :: Char -> Bool
isNotSpace char = ' ' /= char

countCharClass :: (Char -> Bool) -> String -> Int 
countCharClass charClass wordSoFar = length $ filter charClass wordSoFar

charClassIndices :: (Char -> Bool) -> String -> [Int]
charClassIndices charClassP wordSoFar = 
  foldl (\l (c,i) -> if charClassP c then
                   i : l 
                 else
                   l) [] $ zip wordSoFar [0..]

wordOfNCharacters :: [String] -> String -> Int -> String
wordOfNCharacters (word:rest) wordSoFar n
  | length result >= n = take n result
  | otherwise = wordOfNCharacters rest result n
  where result = wordSoFar ++ (filter isNotSpace word)
wordOfNCharacters _ wordSoFar _ = wordSoFar

wordOfLengthN :: [String] -> Int -> String
wordOfLengthN listOfWords n =
  wordOfNCharacters listOfWords "" n

wordOfNChars :: (String -> CharLocation -> String) -> ([Char] -> [Char]) ->
                (Char -> Bool) -> String -> [CharLocation] -> Maybe String
wordOfNChars handlePerIndex permuteCandidate regex candidate replIndices 
  | numToChange == 0 = Just candidate
  | numToChange > length candidate = Nothing
  | numOccurrences == numToChange = Just candidate
  | otherwise = Just $ foldl handlePerIndex (permuteCandidate candidate) replIndices
  where numToChange = length replIndices
        numOccurrences = countCharClass regex candidate

wordOfNCapitals, wordOfNNumbers, wordOfNSpecial :: (Char -> Bool) -> String -> [CharLocation] -> Maybe String
wordOfNCapitals = wordOfNChars capitalizePerIndex (\candidate -> map toLower candidate)
wordOfNNumbers  = wordOfNChars numericPerIndex id
wordOfNSpecial  = wordOfNChars specialPerIndex id

capitalizePerIndex :: String -> CharLocation -> String
capitalizePerIndex word (CharLocation idx replacement) 
  | isLower (word !! idx) = concat [take idx word, [toUpper (word !! idx)], drop (idx + 1) word]
  | otherwise             = concat [take idx word, [replacement], drop (idx + 1) word]

numericPerIndex :: String -> CharLocation -> String
numericPerIndex word (CharLocation idx replacement) 
  | isNumber (word !! idx) = concat [take idx word, [(word !! idx)], drop (idx + 1) word]
  | otherwise              = concat [take idx word, [replacement], drop (idx + 1) word]

specialPerIndex :: String -> CharLocation -> String
specialPerIndex word (CharLocation idx replacement) =
  concat [take idx word, [replacement], drop (idx + 1) word]

uniqueRandomList :: [Int] -> Int -> Int -> Int -> [Int]
uniqueRandomList _ _ _ 0 = []
uniqueRandomList randNums lower upper count =
  take count $ nub $ map (\a -> a `mod` (upper - lower) + lower)
                   (take (count * 50) randNums)

permuteWithCharset :: StdGen -> ((Char -> Bool) -> String -> [CharLocation] -> Maybe String)
                      -> (Char -> Bool) -> [Int] -> String -> String -> String -> Int -> Maybe String
permuteWithCharset _ _ _ _ _ _ candidate 0 = Just candidate
permuteWithCharset gen mainFn regex shuffMatches charset altCharset candidate replacements = 
  let locations = getIndicesToModify gen regex shuffMatches charset altCharset replacements candidate in
  mainFn regex candidate locations

getIndicesToModify :: StdGen -> (Char -> Bool) -> [Int] -> String -> String -> Int -> String -> [CharLocation]
getIndicesToModify gen charCharClass shuffMatches primaryCharset alternateCharset targetNum candidate
  | occurences >  targetNum = 
      let removals = occurences - targetNum in 
      let altChars = map (\i -> CharLocation i $ alternateCharset !! i) replIndices in
      take removals altChars
  | occurences == targetNum = []
  | occurences <  targetNum = findSlots targetNum
  where occurences = countCharClass charCharClass candidate
        replIndices = randomRs (0, (length alternateCharset) - 1) gen
        findSlots additions
          | additions == 0 = []
          | additions > length candidate = []
          | otherwise =
              let candidate_char_idxs = take additions $ shuffMatches in
              zipWith CharLocation candidate_char_idxs primaryCharset 
getIndicesToModify _ _ _ _ _ _ _ = []

generate :: ((Char -> Bool) -> String -> [CharLocation] -> Maybe String)
            -> String -> (Char -> Bool) -> String -> [Char] -> Int -> IO [Char]
generate permuteFn word charsetp charset altCharset count = do
  pt <- getPOSIXTime
  let time = round (pt * 1000000)
  let gen = mkStdGen time
  shuffCharset <- runRVar (shuffle charset) StdRandom
  print shuffCharset
  shuffAltCharset <- runRVar (shuffle altCharset) StdRandom
  print shuffAltCharset
  shuffMatches <- runRVar (shuffle (charClassIndices (not . charsetp) word)) StdRandom
  case permuteWithCharset gen permuteFn charsetp shuffMatches shuffCharset shuffAltCharset word count of
    Nothing -> return ""
    Just x -> return x

attemptCandidate :: String -> UserConfig -> IO [Char]
attemptCandidate baseWord configs = do
  let core = ucSpecialsToUse configs
  let numNums = ucNumNumbers configs 
  let numCaps = ucNumCapitals configs 
  let numSpecials = ucNumSpecials configs
  print $ "baseWord = " ++ baseWord
  wordWithCaps <- generate wordOfNCapitals baseWord letterCharClass capitalCharset stdAlternateCharset numCaps
  print wordWithCaps
  wordWithNums <- generate wordOfNNumbers wordWithCaps numberCharClass numericCharset stdAlternateCharset numNums
  print wordWithNums
  wordWithSpecial <- generate wordOfNSpecial wordWithNums (specialCharClass core) core stdAlternateCharset numSpecials
  print $ "wordWithSpecial = " ++ wordWithSpecial
  if baseWord == wordWithSpecial then
    return wordWithSpecial
  else
    attemptCandidate wordWithSpecial configs

parseArgs :: [String] -> UserConfig -> IO UserConfig
parseArgs [] config = return config
parseArgs args config
  | flag : pwordLength : rest          <- args
  , elem flag ["-l", "--length"]
  = parseArgs rest
  $ config { ucPwordLength = read pwordLength }
  
  | flag : numCapitals : rest          <- args
  , elem flag ["-c", "--capitals"]
  = parseArgs rest
  $ config { ucNumCapitals = read numCapitals }
  
  | flag : numNumbers : rest          <- args
  , elem flag ["-n", "--numbers"]
  = parseArgs rest
  $ config { ucNumNumbers = read numNumbers }
  
  | flag : numSpecials : rest          <- args
  , elem flag ["-s", "--specials"]
  = parseArgs rest
  $ config { ucNumSpecials = read numSpecials }
   
  | flag : specialsToUse : rest          <- args
  , elem flag ["-u", "--use_specials"]
  = parseArgs rest
  $ config { ucSpecialsToUse = specialsToUse }
 
  | otherwise
  = error $ "Cannot parse arguments " ++ show args

main :: IO ()
main = do
  argv <- getArgs
  configs <- parseArgs argv userConfigDefault
  file <- readFile "wordlist.txt"
  let listOfWords = map (filter (not . isCR)) (lines file)
  shuffledWords <- runRVar (shuffle listOfWords) StdRandom
  let word = wordOfLengthN shuffledWords $ ucPwordLength configs
  final <- attemptCandidate word configs
  print final
