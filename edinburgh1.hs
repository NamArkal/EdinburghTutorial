---Problem 1
halveEvens :: [Int] -> [Int]
halveEvens xs = [div x 2| x <- xs, even x]

---Problem 2
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = [] 
halveEvensRec (x:xs) | even x = [div x 2]++ halveEvensRec(xs)
                     | otherwise = halveEvensRec(xs)

---Problem 3
inRange :: Int ->  Int -> [Int] -> [Int]
inRange lo hi xs = [x|x <- xs, x>=lo && x <= hi ]

---Problem 4
inRangeRec :: Int ->  Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs) | (x>=lo) && (x<=hi) = [x] ++ inRangeRec lo hi (xs)
                        | otherwise = inRangeRec lo hi (xs)                     

---Problem 5
countPositives :: [Int] -> Int
countPositives xs = length[x | x <- xs , x > 0]

---Problem 6
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | (x>0) = 1+ ( countPositivesRec (xs) )
                         | otherwise = countPositivesRec (xs)

--- Problem 7
-- Helper function
discount :: Int -> Int
discount price = round(fromIntegral(price) * 0.9)

-- List-comprehension version of pennypincher
pennypincher :: [Int] -> Int
pennypincher prices = sum [discount x | x <- prices, discount x <= 19900]

--- Problem 8
-- Helper function
discount :: Int -> Int
discount price = round(fromIntegral(price) * 0.9)

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (price:prices) | (discount price) <= 19900 = discount price + pennypincherRec prices
                               | otherwise = pennypincherRec prices

--- Problem 9
multDigits :: String -> Int
multDigits str = product [digitToInt c | c <- str, isDigit c]

--- Problem 10
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x = (digitToInt x) * multDigitsRec xs
                     | otherwise = multDigitsRec xs

--- Problem 11                   
capitalise :: String -> String
capitalise [] = []
capitalise str = (toUpper (head str)) : [toLower y | y <- tail str]

--- Problem 12
-- Recursive version
capitaliseRec :: String -> String
capitaliseRec [] = []
capitaliseRec (x:xs) = toUpper x : lowerRec xs

-- Helper function
lowerRec :: String -> String
lowerRec [] = []
lowerRec (x:xs) = toLower x : lowerRec xs