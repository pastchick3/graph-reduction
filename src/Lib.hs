module Lib (compile) where

import FlParser (parseFl)

compile :: String -> Either String String
compile src = do
    flProg <- parseFl src
    return $ show flProg
