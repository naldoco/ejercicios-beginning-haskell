{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Monad (forM_)

main =
  forM_ [ [['a'], ['a', 'b']]
        , [[], ['a', 'b']]
        , [[]]
        , []
        ] $ \list -> do
    putStrLn $ concat [show list, " desugared is ", desugarListOfLists list]
    putStrLn $ concat ["is ", show list, " empty? ", if isEmpty list then "YES" else "NO"]
    putStrLn $ concat ["is ", show list, " a singleton list? ", if isSingleton list then "YES" else "NO"]
    putStrLn $ concat [show list, " concatenated is ", show $ concatListOfLists list]
    putStrLn ""


desugarListOfLists xs = if null xs
                        then "[]"
                        else desugarList xs show


desugarList xs f = if null xs
                   then "[]"
                   else concat ["(", f (head xs), ")", ":", desugarList (tail xs) f]


isEmpty xs = if null xs
             then True
             else null (head xs)


isSingleton xs = if null xs
                 then False
                 else True && not (isSingleton (tail xs))


concatListOfLists xs = if null xs
                       then []
                       else head xs ++ concatListOfLists (tail xs)


maxmin list = if null (tail list)
              then (head list, head list)
              else ( if (head list) > fst (maxmin (tail list))
                     then head list
                     else fst (maxmin (tail list))
                   , if (head list) < snd (maxmin (tail list))
                     then head list
                     else snd (maxmin (tail list))
                   )


maxmin2 list = let h = head list
               in
                 if null (tail list)
                 then (h, h)
                 else ( if h > t_max then h else t_max
                      , if h < t_min then h else t_min )
  where t = maxmin2 (tail list)
        t_max = fst t
        t_min = snd t


data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool

data Gender = Male | Female | Other


data Person = Person String String Gender


instance Show Gender
instance Show Person


clientName :: Client -> String
clientName client = case client of
  GovOrg name -> name
  Company name id person resp -> name
  Individual person ads ->
    case person of
      Person fName lName gender -> fName ++ " " ++ lName


clientName2 :: Client -> String
clientName2 client = case client of
  -- other patterns
  Individual (Person fName lName _) _ -> fName ++ " " ++ lName


clientName3 (GovOrg name)                         = name
clientName3 (Company name _ _ _)                  = name
clientName3 (Individual (Person fName lName _) _) = fName ++ " " ++ lName


companyName :: Client -> String
companyName client = case client of  -- <- Pattern matching non-exhaustive
  Company name _ _ _ -> name


companyName2 :: Client -> Maybe String
companyName2 client = case client of
  Company name _ _ _ -> Just name
  _ -> Nothing


f :: Client -> String
f client = case client of
             Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
             _                                  -> "There is no boss"
g :: Client -> String
g client = case client of
             Company _ _ (Person name _ _) pos ->
               case pos of "Boss" -> name ++ " is the boss"
             _                               -> "There is no boss"


fibonacci :: Integer -> Integer
fibonacci n = case n of
                0 -> 0
                1 -> 1
                _ -> fibonacci (n-1) + fibonacci (n-2)


fibonacci2 0 = 0
fibonacci2 1 = 1
fibonacci2 n = fibonacci (n-1) + fibonacci (n-2)
