module Lib (compile) where

import FlParser qualified (parse)

compile :: String -> Either String String
compile src = do
    flProg <- FlParser.parse src
    return $ show flProg
