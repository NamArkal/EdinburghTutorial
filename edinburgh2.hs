--- 1
rotate :: Int -> [Char] -> [Char]
rotate k list | k < 0           = error "Negative numbers not allowed."
              | k > length list = error "Length too large."
              | otherwise       = take (length list) (drop k (cycle list))
              
--- 2
makeKey :: Int -> [(Char,Char)]
makeKey n = zip ['A'..'Z'] (rotate n ['A'..'Z'])

--- 3
lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec c [] = c
lookUpRec c (t:tuples) | c == fst t = snd t
                       | otherwise  = lookUpRec c tuples
                       
--- 4
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c tuples | null matches = c
                | otherwise    = head matches
                where
                  matches      = [snd t | t <- tuples, fst t == c]
                  
--- 5
encipher :: Int -> Char -> Char
encipher k c = lookUp c (makeKey k)

--- 6
normalize :: String -> String
normalize s = [ toUpper c | c <- s, isAlphaNum c ]

--- 7
encipherStr :: Int -> String -> String
encipherStr k str = [ encipher k c | c <- normalize str ]

--- 8
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey [] = []
reverseKey tuples = [ (snd t,fst t) | t <- tuples ]

--- 9
reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec []= []
reverseKeyRec (t:tuples) = [(snd t,fst t)] ++ reverseKeyRec tuples

--- 10
rotate :: Int -> [Char] -> [Char]
rotate k list | k < 0           = error "Negative numbers not allowed."
              | k > length list = error "Length too large."
              | otherwise       = take (length list) (drop (26-k) (cycle list))

makeKey :: Int -> [(Char,Char)]
makeKey n = zip ['A'..'Z'] (rotate n ['A'..'Z'])

lookUp :: Char -> [(Char, Char)] -> Char
lookUp c tuples | null matches = c
                | otherwise    = head matches
                where
                  matches      = [snd t | t <- tuples, fst t == c]

decipher :: Int -> Char -> Char
decipher k c = lookUp c (makeKey k)

normalize :: String -> String
normalize s = [ toUpper c | c <- s, isAlphaNum c ]

decipherStr :: Int -> String -> String
decipherStr k str = [ decipher k c | c <- normalize str ]

--- 11
contains :: String -> String -> Bool
contains str s = if isInfixOf s str
                 then True
                 else False

--- 12
candidates :: String -> [(Int, String)]
candidates txt = [ (k,str) | (k,str) <- pairs, contains str "AND" || contains str "THE"]
                 where pairs = [(k,decipherStr k txt) | k <- [0..25]]