import Advent
getWindows (x:y:z:xs) = (x + y + z):getWindows (y:z:xs)
getWindows _ = []

main = do
  f     <- readFile "inputs/day1.txt"
  let ns = (read <$> lines f) :: [Integer]
  print $ length $ filter (uncurry (<)) $ pairs ns
  print $ length $ filter (uncurry (<)) $ pairs $ getWindows ns

