import Data.List (sort)

{- True if the word is compatible with the wildcard regex. -}
matchesExact :: String -> String -> Bool
matchesExact exact word =
  all (uncurry pred) (zip exact word)
  where
    pred '.' _ = True
    pred x y = x == y

{- Filter characters that match the wildcard placement. -}
filterWild :: String -> String -> String
filterWild exact word =
  map snd $ filter (uncurry pred) (zip exact word)
  where
    pred '.' _ = True
    pred _ _ = False

{-
True if "must have" characters occur among the wild characters.

This works by sorting the "must have" characters and the wild characters,
and checking that the "must have" is a sub list.
-}
matchesExists :: String -> String -> String -> Bool
matchesExists exact exists word =
  isSublist exists' word'
  where
    exists' = sort exists
    word' = sort $ filterWild exact word

    isSublist [] _ = True
    isSublist _ [] = False
    isSublist (x:xs) (y:ys)
      | x == y = isSublist xs ys
      | otherwise = isSublist (x:xs) ys

{- True if never characters do not occur among the wild characters. -}
matchesNever :: String -> String -> String -> Bool
matchesNever exact never word =
  all pred never
  where
    pred c = not $ elem c word'
    word' = filterWild exact word

matches :: String -> String -> String -> String -> Bool
matches exact exists never word =
  matchesExact exact word &&
  matchesExists exact exists word &&
  matchesNever exact never word

matchingWords :: String -> String -> String -> [String] -> [String]
matchingWords exact exists never =
  filter (matches exact exists never)

main :: IO ()
main = do
  dict <- lines <$> readFile "dict.txt"
  let exact = ".oi.t"
  let exists = ""
  let never = "dreamvuchlfyjs"
  mapM_ print $ matchingWords exact exists never dict
