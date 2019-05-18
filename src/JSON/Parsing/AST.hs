module JSON.Parsing.AST
    ( JValue(..)
    )
where

import qualified Data.Map                      as M

data JValue
    = JObject (M.Map String JValue)
    | JList [JValue]
    | JString String
    | JInt Integer
    | JFloat Double
    deriving (Show)
