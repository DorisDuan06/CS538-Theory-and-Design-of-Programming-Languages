import Data.List
import Data.Char

-- Numbers to Words
--
--
--
-- This program translates numbers to English words. We will assume that the
-- number is at most 6 digits, and is non-negative. (The program reports errors
-- if the input is not of this form.) The final function:
--
-- printNum 123456
--
-- should return
--
-- "one hundred twenty-three thousand four hundred fifty-six"
--
-- (Note the dashes.) As before we will build up to this function by first
-- building printing functions for one digit, two digit, and three digit
-- numbers.
--
-- We will use three lists of number names:

ones = [ "zero", "one", "two", "three", "four"
       , "five", "six", "seven", "eight", "nine"]
teens = [ "ten", "eleven", "twelve", "thirteen", "fourteen"
        , "fifteen" , "sixteen", "seventeen", "eighteen", "nineteen" ]
tens = [ "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety" ]

-- The indexing operation `!!` will be useful to get elements. For instance:
--
-- ones !! 0
--
-- returns "zero", while
--
-- tens !! 3
--
-- returns "fifty". Note that the first element is at index 0.

-- Each function that prints numbers takes an Int and returns a string.
-- This function is called by main. Redefine it to printNum2, printNum3, and
-- printNum6.

printNum :: Int -> String
printNum = printNum1

-- printNum1 function works for all single-digit numbers: 0, 1, ..., 9

printNum1 :: Int -> String
printNum1 n
    | 0 <= n && n < 10 = ones !! n
    | otherwise        = printNum2 n

-- printNum2 function handles numbers with at most 2 digits. It works on
-- numbers 0, 1, ..., 99. For instance:
--
-- printNum2 0  == "zero"
-- printNum2 10 == "ten"
-- printNum2 15 == "fifteen"
-- printNum2 34 == "thirty-four"

printNum2 :: Int -> String
printNum2 n
    | 0 <= n && n < 10   = printNum1 n
    | 10 <= n && n < 20  = teens !! (n `mod` 10)
    | 10 <= n && n < 100 && (n `mod` 10) == 0  = tens !! (n `div` 10 - 2)
    | 20 < n && n < 100  = tens !! (n `div` 10 - 2) ++ "-" ++ printNum1 (n `mod` 10)
    | otherwise          = printNum3 n

-- printNum3 function handles numbers with at most 3 digits. It works on
-- numbers 0, 1, ..., 999. It reuses printNum2. For instance:
--
-- printNum3 15  == "fifteen"
-- printNum3 101 == "one hundred one"
--
-- Note that we don't put an "and" in "one hundred one".

printNum3 :: Int -> String
printNum3 n
    | 0 <= n && n < 10    = printNum1 n
    | 10 <= n && n < 100  = printNum2 n
    | 100 <= n && n < 1000 && (n `mod` 100) == 0 = ones !! (n `div` 100) ++ " hundred"
    | 100 < n && n < 1000 = ones !! (n `div` 100) ++ " hundred " ++ printNum2 (n `mod` 100)
    | otherwise           = printNum6 n

-- Finally, printNum6 function prints numbers up to six digits: 0, 1, ...,
-- 999999. It reuses printNum3. For instance:
--
-- printNum 123000 == "one hundred twenty-three thousand"
-- printNum 950001 == "nine hundred fifty thousand one"
--
-- We don't put an "and", but it is not hard to add this extension.

printNum6 :: Int -> String
printNum6 n
    |  0 <= n && n < 10        = printNum1 n
    | 10 <= n && n < 100       = printNum2 n
    | 100 < n && n < 1000      = printNum3 n
    | 1000 <= n && n < 1000000 && (n `mod` 1000) == 0 = printNum3 (n `div` 1000) ++ " thousand"
    | 1000 <= n && n < 1000000 = printNum3 (n `div` 1000) ++ " thousand " ++ printNum3 (n `mod` 1000)
    | otherwise                = "Number not within 0 ~ 999999!"

-- An infinite loop that keeps asking you for a number to convert to English
-- words. It calls the printNum function, which is defined on line 64: you may
-- wish to first assign it to printNum1, and then assign it to printNum2,
-- printNum3, and printNum6 as you complete those functions. It loops infinitely,
-- asking for input. You can terminate it with CTRL-C or CTRL-D.

main = do
    putStrLn "-- Please enter a number."
    int_s <- getLine
    let x = read int_s
    print (printNum x)
    main
