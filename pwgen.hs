module Main where

import Data.Random
import Data.Char
import Data.List
import Data.Time.Clock.POSIX
import System.Environment
import System.Random
import System.FilePath.Posix

data CharLocation  = CharLocation Int Char
                     deriving (Show)
type CharLocations = [CharLocation]
data UserConfig = UserConfig { 
                                ucPwordLength   :: Int,
                                ucNumCapitals   :: Maybe Int,
                                ucMinCapitals   :: Maybe Int,
                                ucMaxCapitals   :: Maybe Int,
                                ucNumNumbers    :: Maybe Int, 
                                ucMinNumbers    :: Maybe Int,
                                ucMaxNumbers    :: Maybe Int,
                                ucNumSpecials   :: Maybe Int,
                                ucMinSpecials   :: Maybe Int,
                                ucMaxSpecials   :: Maybe Int,
                                ucSpecialsToUse :: String,
                                ucHelpRequested :: Bool
                             } deriving (Show)
userConfigDefault :: UserConfig
userConfigDefault = UserConfig { 
                      ucPwordLength = 24, 
                      ucNumCapitals = Nothing, 
                      ucMinCapitals = Just 1,
                      ucMaxCapitals = Just 3,
                      ucNumNumbers = Nothing,
                      ucMinNumbers = Just 1, 
                      ucMaxNumbers = Just 1, 
                      ucNumSpecials = Nothing,
                      ucMinSpecials = Just 1,
                      ucMaxSpecials = Just 2,
                      ucSpecialsToUse = "._-",
                      ucHelpRequested = False
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
  | isLower $ word !! idx = concat [take idx word, [toUpper (word !! idx)], drop (idx + 1) word]
  | otherwise             = concat [take idx word, [replacement], drop (idx + 1) word]

numericPerIndex :: String -> CharLocation -> String
numericPerIndex word (CharLocation idx replacement) 
  | isNumber $ word !! idx = concat [take idx word, [(word !! idx)], drop (idx + 1) word]
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
      let indexesToRemove = take removals shuffMatches in
      let altChars = map (\(rmv, i) -> CharLocation rmv $ alternateCharset !! i) $ zip indexesToRemove replIndices in
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
  --print shuffCharset
  shuffAltCharset <- runRVar (shuffle altCharset) StdRandom
  --print shuffAltCharset
  shuffMatches <- runRVar (shuffle (charClassIndices (not . charsetp) word)) StdRandom
  case permuteWithCharset gen permuteFn charsetp shuffMatches shuffCharset shuffAltCharset word count of
    Nothing -> return ""
    Just x -> return x

generateNumberOfNumbers :: UserConfig -> IO Int
generateNumberOfNumbers configs =
  case ucNumNumbers configs of
    Just x    -> return x
    Nothing   -> case (ucMinNumbers configs, ucMaxNumbers configs) of
                   (Nothing, Nothing) -> return 0
                   (Nothing, Just y)  -> runRVar (uniform 0 y) StdRandom
                   (Just x, Nothing)  -> return x
                   (Just x, Just y)   -> runRVar (uniform x y) StdRandom

generateNumberOfCapitals :: UserConfig -> IO Int
generateNumberOfCapitals configs =
  case ucNumCapitals configs of
    Just x    -> return x
    Nothing   -> case (ucMinCapitals configs, ucMaxCapitals configs) of
                   (Nothing, Nothing) -> return 0
                   (Nothing, Just y)  -> runRVar (uniform 0 y) StdRandom
                   (Just x, Nothing)  -> return x
                   (Just x, Just y)   -> runRVar (uniform x y) StdRandom

generateNumberOfSpecials :: UserConfig -> IO Int
generateNumberOfSpecials configs =
  case ucNumSpecials configs of
    Just x    -> return x
    Nothing   -> case (ucMinSpecials configs, ucMaxSpecials configs) of
                   (Nothing, Nothing) -> return 0
                   (Nothing, Just y)  -> runRVar (uniform 0 y) StdRandom
                   (Just x, Nothing)  -> return x
                   (Just x, Just y)   -> runRVar (uniform x y) StdRandom

attemptCandidate :: UserConfig -> String -> Int -> Int -> Int -> IO [Char]
attemptCandidate configs baseWord numCaps numNums numSpecials = do
  let core = ucSpecialsToUse configs
  --print $ "baseword = " ++ baseWord
  wordWithCaps <- generate wordOfNCapitals baseWord letterCharClass capitalCharset stdAlternateCharset numCaps
  --print wordWithCaps
  wordWithNums <- generate wordOfNNumbers wordWithCaps numberCharClass numericCharset stdAlternateCharset numNums
  --print wordWithNums
  wordWithSpecial <- generate wordOfNSpecial wordWithNums (specialCharClass core) core stdAlternateCharset numSpecials
  --print $ "wordWithSpecial = " ++ wordWithSpecial
  if baseWord == wordWithSpecial then
    return wordWithSpecial
  else
    attemptCandidate configs wordWithSpecial numCaps numNums numSpecials

parseArgs :: [String] -> UserConfig -> IO UserConfig
parseArgs [] config = return config
parseArgs args config
  | flag : pwordLength : rest          <- args
  , elem flag ["-l", "--length"]
  = parseArgs rest
  $ config { ucPwordLength = read pwordLength }
  
  | flag : numNumbers : rest          <- args
  , elem flag ["-n", "--numbers"]
  = parseArgs rest
  $ config { ucNumNumbers = read numNumbers }
  
  | flag : minNumbers : rest          <- args
  , elem flag ["-mn", "--min-numbers"]
  = parseArgs rest
  $ config { ucMinNumbers = Just $ read minNumbers }
  
  | flag : maxNumbers : rest          <- args
  , elem flag ["-xn", "--max-numbers"]
  = parseArgs rest
  $ config { ucMaxNumbers = Just $ read maxNumbers }
  
  | flag : numCapitals : rest          <- args
  , elem flag ["-c", "--capitals"]
  = parseArgs rest
  $ config { ucNumCapitals = read numCapitals }
  
  | flag : minCapitals : rest          <- args
  , elem flag ["-mc", "--min-capitals"]
  = parseArgs rest
  $ config { ucMinNumbers = Just $ read minCapitals }
  
  | flag : maxCapitals : rest          <- args
  , elem flag ["-xc", "--max-capitals"]
  = parseArgs rest
  $ config { ucMaxNumbers = Just $ read maxCapitals }
  
  | flag : numSpecials : rest          <- args
  , elem flag ["-s", "--specials"]
  = parseArgs rest
  $ config { ucNumSpecials = Just $ read numSpecials }
  
  | flag : minSpecials : rest          <- args
  , elem flag ["-xs", "--min-specials"]
  = parseArgs rest
  $ config { ucMinSpecials = Just $ read minSpecials }
  
  | flag : maxSpecials : rest          <- args
  , elem flag ["-ms", "--max-specials"]
  = parseArgs rest
  $ config { ucMaxSpecials = Just $ read maxSpecials }
   
  | flag : specialsToUse : rest          <- args
  , elem flag ["-u", "--use_specials"]
  = parseArgs rest
  $ config { ucSpecialsToUse = specialsToUse }
   
  | flag : rest          <- args
  , elem flag ["-h", "--help"]
  = parseArgs rest
  $ config { ucHelpRequested = True }
 
  | otherwise
  = error $ "Cannot parse arguments " ++ show args

main :: IO ()
main = do
  argv <- getArgs
  configs <- parseArgs argv userConfigDefault
  if ucHelpRequested configs == True then do
    print "pwgen help"
    print "----------"
    print "Usage: pwgen [options]"
    print "  Options:"
    print "    -l <length>| --length <length>"
    print "      Specify the desired length of the password."
    print "    -c <count> | --capitals <count>"
    print "      Specify the count of capital letters to use"
    print "      in the password, or use default"
    print "    -n <count> | --numbers <count>"
    print "      Specify the count of numeric digits to use"
    print "      in the password, or use default"
    print "    -s <count> | --specials <count>"
    print "      Specify the count of special characters (such as"
    print "      %, $, *, !) to use in the password, or use default"
    print "    -u <charlist> | --use_specials <charlist>"
    print "      Specify the character list to use for special"
    print "      characters"
  else do
    binpath <- getExecutablePath
    let path = takeDirectory binpath
    --print $ path ++ "/wordlist.txt"
    file <- readFile $ path ++ "/wordlist.txt"
    let listOfWords = map (filter (not . isCR)) (lines file)
    shuffledWords <- runRVar (shuffle listOfWords) StdRandom
    let word = wordOfLengthN shuffledWords $ ucPwordLength configs
    numNums <- generateNumberOfNumbers configs
    numCaps <- generateNumberOfCapitals configs 
    numSpecials <- generateNumberOfSpecials configs
    --print $ "baseWord = " ++ word
    --print $ "configs = " ++ show configs
    print $ "numNums = " ++ show numNums
    print $ "numCaps = " ++ show numCaps
    print $ "numSpecials = " ++ show numSpecials
    final <- attemptCandidate configs word numCaps numNums numSpecials
    print final
