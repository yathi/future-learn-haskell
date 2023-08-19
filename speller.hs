import Data.List (intercalate)
speller :: [[Char]] -> [Char]
speller dictionary = intercalate ", " (map parse dictionary)

parse :: [Char] -> [Char]
parse (x:xs) = [x] ++ " is for " ++ x:xs
parse [] = ""


-- speller dictionary = foldl (\elt acc -> elt ++ ", " ++ acc) "" (map parse dictionary)

-- Task
-- speller ["abacus"] -- > "a is for abacus"
-- speller [] -- > ""
-- speller ["apple", "banana", "coconut"] 
--  -- > "a is for apple, b is for banana, and c is for coconut"
-- speller ["whisky", "x-ray"]
--  -- > "w is for whisky, and x is for x-ray"