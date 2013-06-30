module HaSnip.Misc where

import Data.Char

 -- | Converts "CamelCase" to C-style "camel_case" for use in template haskell
convertSymbolName :: String -> String
convertSymbolName [] = []
convertSymbolName (a:[]) = (toLower a):[]
convertSymbolName (a:b:cs)
  | isUpper b = (toLower a):'_':rest
  | otherwise = (toLower a)    :rest
  where rest = convertSymbolName $ b:cs
