module Lib (compile) where

import FlParser (parseFl)
import ElcTranslator (translateElc)

compile :: String -> Either String String
compile src = do
    flProg <- parseFl src
    elcExp <- translateElc flProg
    return $ show elcExp
