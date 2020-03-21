import Data.List
import Data.Char
import Data.String

-- Frequent Words
--
--
--
-- This program has a function freqWords that takes a number n and a string and
-- prints the n most frequent words along with how often they appear. The
-- function should ignore case ("bat" and "BAT" should be treated as the same
-- word) and if there are ties, print out words in alphabetical order. For
-- instance:
--
-- freqWords 3 "cat bat Foo xray Bat YoYo"
--
-- should return the string
--
-- "bat: 2, cat: 1, foo: 1"
--
-- Note that we count two occurrences of "bat", and we do not print "xray", and
-- "yoyo" since they are after "cat" and "foo" alphabetically.

-- First, makeLower function takes a string, and converts it to lower case.

makeLower :: String -> String
makeLower str = [ toLower x | x <- str ]

-- Next, splitSpace function splits up a string into a list of strings, breaking
-- at each space.

splitSpace :: String -> [String]
splitSpace str = words str

-- Then, define a function that sorts a list of strings alphabetically.

sortAlpha :: [String] -> [String]
sortAlpha str = sort str

-- Now, we can collect "runs" of identical words into a word and its count. For
-- instance: 
--
-- countRuns ["a", "a", "b", "c", "c", "c"]
--
-- should return
--
-- [("a", 2), ("b", 1), ("c", 3)]

countRuns :: [String] -> [(String, Int)]
countRuns [] = []
countRuns (x:xs) = (x, b+1) : (countRuns (drop b xs))
                   where b = length(filter (== x) xs)

-- Next, we can sort the frequency list in decreasing order of frequency.
-- For instance:
--
-- sortFreqs [("a", 2), ("b", 1), ("c", 3)]
--
-- should return
--
-- [("c", 3), ("a", 2), ("b", 1)]

sortFreqs :: [(String, Int)] -> [(String, Int)]
sortFreqs str = sortBy (\(_,a) (_,b) -> compare b a) str

-- Now, time for tidying up. printFreqs function converts the frequency list
-- into a printable form:
--
-- printFreqs [("c", 3), ("a", 2), ("b", 1)]
--
-- should return
--
-- ["c: 3", "a: 2", "b: 1"]

printFreqs :: [(String, Int)] -> [String]
printFreqs str = map (\(x,y) -> x ++ ": " ++ show (y)) str

-- Finally, glue all the pieces together to build the final function.

freqWords :: Int -> String -> String
freqWords n str = intercalate ", " (take n (printFreqs $ sortFreqs $ countRuns $ sortAlpha $ splitSpace $ makeLower str))

-- A simple main function that asks you for a string of words and how many of
-- the most frequent words you want

main = do
    putStrLn "-- What text would you like to find the frequent words of?"
    putStrLn "-- (Enter all on one line.)"
    words <- getLine
    putStrLn "-- How many of the most frequent words would you like?"
    int_s <- getLine
    let x = read int_s
    print (freqWords x words)
