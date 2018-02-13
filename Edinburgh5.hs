--- 1
data Fruit = Apple(String, Bool) | Orange(String, Int)
             deriving (Show)

isBloodOrange :: Fruit -> Bool 
isBloodOrange (Apple (_,_)) = False
isBloodOrange (Orange (x,_)) | x `elem` ["Tarocco", "Moro", "Sanguinello"] = True
                             | otherwise = False
                             
--- 2
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments [] = 0
bloodOrangeSegments (fruit:fruitList)
    | isBloodOrange fruit = case fruit of Orange (_,segments) -> segments + bloodOrangeSegments fruitList
    | otherwise           = bloodOrangeSegments fruitList
    
--- 3
worms :: [Fruit] -> Int
worms [] = 0
worms (f:fruitList)
    | containsWorm f = 1 + worms fruitList
    | otherwise = worms fruitList
        where containsWorm (Apple (_,True)) = True
              containsWorm f = False