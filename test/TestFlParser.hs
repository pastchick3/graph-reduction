module TestFlParser (testFlParser) where

import FlDef qualified as Fl
import FlParser (parseFl)
import TestRunner (runTest)

runFlParserTest :: String -> Fl.Prog -> IO ()
runFlParserTest = runTest "FlParser" parseFl

testFlParser :: IO ()
testFlParser = do
    runFlParserTest
        "\
        \ a = a \n\
        \ 1 = 1 \n\
        \ 'a' = 'a' \n\
        \ True = True \n\
        \ False = False \n\
        \ \"a\" = \"a\" \n\
        \ [] = [] \n\
        \ [1] = [1] \n\
        \ [1, 2] = [1, 2] \n\
        \ (1, 2) = (1, 2) \n\
        \ T = T \n\
        \ T 1 = T 1 \n\
        \  \n\
        \ a = (1) \n\
        \ [a | b] \n\
        \ [a | b, c <- d] \n\
        \ case a of \n\
        \     1 -> 1 \n\
        \     1 | True -> 1 \n\
        \       | False -> 1 \n\
        \ f a = 1 \n\
        \ f a | True = 1 \n\
        \ a = f 1 \n\
        \  \n\
        \ a = -1 + 1 - 1 * 1 / 1 \n\
        \ a = 1 == 1 \n\
        \ a = 1 /= 1 \n\
        \ a = 1 < 1 \n\
        \ a = 1 <= 1 \n\
        \ a = 1 > 1 \n\
        \ a = 1 >= 1 \n\
        \ a = True && not True || True \n\
        \ a = (1 : [] ++ []) !! 0 \n\
        \  \n\
        \ a :: a \n\
        \ a :: Int \n\
        \ a :: Char \n\
        \ a :: Bool \n\
        \ a :: String \n\
        \ a :: [Int] \n\
        \ a :: (Int, Int) \n\
        \ a :: Int -> Int \n\
        \ data T = A \n\
        \ data T a = A a | B \n\
        \"
        ( Fl.Prog
            [ Fl.DefVar (Fl.PatVar (Fl.LowerVar "a")) (Fl.ExpVar (Fl.LowerVar "a"))
            ]
        )
