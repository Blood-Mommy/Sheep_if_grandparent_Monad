---------------------------------------------------------------------------------------------------------------------------------
-- Laboratory work 2 (Variant 2)
-- Task: Simple sheep lineage check (is grandparent)
---------------------------------------------------------------------------------------------------------------------------------

-- Define data type for a sheep
data Sheep = Sheep { sheepId :: Int, name :: String, children :: [Int] }
  deriving (Show, Eq)

-- Example sheep family
sheepFamily :: [Sheep]
sheepFamily =
  [ Sheep 1 "Wooly" [2, 3]
  , Sheep 2 "Fluffy" [4]
  , Sheep 3 "Cotton" []
  , Sheep 4 "Snow" []
  ]

---------------------------------------------------------------------------------------------------------------------------------
-- Find a sheep by its ID
---------------------------------------------------------------------------------------------------------------------------------
findSheep :: Int -> [Sheep] -> Maybe Sheep
findSheep sid [] = Nothing
findSheep sid (s:ss)
  | sheepId s == sid = Just s
  | otherwise        = findSheep sid ss

---------------------------------------------------------------------------------------------------------------------------------
-- Check if a sheep is a grandparent
---------------------------------------------------------------------------------------------------------------------------------
isGrandparent :: Int -> [Sheep] -> Bool
isGrandparent sid family =
  case findSheep sid family of
    Nothing -> False  -- sheep not found
    Just sheep ->
      let kids = children sheep
          grandkids = concatMap (\kidId ->
                          case findSheep kidId family of
                            Just kid -> children kid
                            Nothing  -> []
                        ) kids
      in not (null grandkids)  -- if any grandkids exist

---------------------------------------------------------------------------------------------------------------------------------
-- Main part: user interaction
---------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Sheep family:"
  mapM_ print sheepFamily

  putStrLn "\nEnter sheep ID to check if it is a grandparent:"
  input <- getLine
  let sid = read input :: Int
  if isGrandparent sid sheepFamily
     then putStrLn "Yes! This sheep is a grandparent 🐑👴"
     else putStrLn "No, this sheep is not a grandparent."
