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
  [ Sheep 1 "Julija" [2, 3]
  , Sheep 2 "Angelina" [4]
  , Sheep 3 "Marina" []
  , Sheep 4 "Gleb" []
  ]

---------------------------------------------------------------------------------------------------------------------------------
-- Find a sheep by its ID
---------------------------------------------------------------------------------------------------------------------------------
findSheep :: Int -> [Sheep] -> Maybe Sheep
findSheep sheepmb [] = Nothing
findSheep sheepmb (s:ss)
  | sheepId s == sheepmb = Just s
  | otherwise        = findSheep sheepmb ss

---------------------------------------------------------------------------------------------------------------------------------
-- Check if a sheep is a grandparent
---------------------------------------------------------------------------------------------------------------------------------
isGrandparent :: Int -> [Sheep] -> Bool
isGrandparent sheepmb family =
  case findSheep sheepmb family of
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
-- Main part
---------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Sheep family:"
  mapM_ print sheepFamily

  putStrLn "\nPlease, write sheep ID:"
  input <- getLine
  let sheepmb = read input :: Int
  if isGrandparent sheepmb sheepFamily
     then putStrLn "This sheep is a grandparent"
     else putStrLn "This sheep is not a grandparent."
