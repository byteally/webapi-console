-- | 

module WebApi.Console.Function where

import Data.List

listNull :: [a] -> Bool
listNull = null

listAnd :: [Bool] -> Bool
listAnd = and

listOr :: [Bool] -> Bool
listOr = or

listElem :: Eq a => a -> [a] -> Bool
listElem = elem

listNotElem :: Eq a => a -> [a] -> Bool
listNotElem = notElem
