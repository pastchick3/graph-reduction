module TestFlParser (testFlParser) where

import FlDef
import FlParser (parseFl)
import TestRunner (runTest)

runFlParserTest :: String -> Prog -> IO ()
runFlParserTest = runTest "FlParser" parseFl

testFlParser :: IO ()
testFlParser = do
    runFlParserTest
        "\
        \ a = a           \n\
        \ 1 = 1           \n\
        \ 'a' = 'a'       \n\
        \ True = True     \n\
        \ False = False   \n\
        \ \"a\" = \"a\"   \n\
        \ [] = []         \n\
        \ [1] = [1]       \n\
        \ [1, 2] = [1, 2] \n\
        \ (1, 2) = (1, 2) \n\
        \ T = T           \n\
        \ T 1 = T 1       \n\
        \"
        ( Prog
            [ DefVar (PatVar (LowerVar "a")) (ExpVar (LowerVar "a"))
            , DefVar (PatInt 1) (ExpInt 1)
            , DefVar (PatChar 'a') (ExpChar 'a')
            , DefVar (PatBool True) (ExpBool True)
            , DefVar (PatBool False) (ExpBool False)
            , DefVar (PatStr "a") (ExpStr "a")
            , DefVar (PatList []) (ExpList [])
            , DefVar (PatList [PatInt 1]) (ExpList [ExpInt 1])
            , DefVar (PatList [PatInt 1, PatInt 2]) (ExpList [ExpInt 1, ExpInt 2])
            , DefVar (PatTuple [PatInt 1, PatInt 2]) (ExpTuple [ExpInt 1, ExpInt 2])
            , DefVar (PatCtor (Ctor (UpperVar "T") [])) (ExpAdt (Ctor (UpperVar "T") []))
            , DefVar (PatCtor (Ctor (UpperVar "T") [PatInt 1])) (ExpAdt (Ctor (UpperVar "T") [ExpInt 1]))
            ]
        )
    runFlParserTest
        "\
        \ a = (1)             \n\
        \ a = [b | c]         \n\
        \ a = [b | c, d <- e] \n\
        \ a = case b of       \n\
        \     1 -> 1          \n\
        \     1 | True -> 1   \n\
        \       | False -> 1  \n\
        \   where             \n\
        \     a = 1           \n\
        \ f a = 1             \n\
        \ f a | True = 1      \n\
        \ a = f 1             \n\
        \"
        ( Prog
            [ DefVar (PatVar (LowerVar "a")) (ExpGroup (ExpInt 1))
            , DefVar (PatVar (LowerVar "a")) (ExpListComp (ExpVar (LowerVar "b")) [Guard (ExpVar (LowerVar "c"))])
            , DefVar (PatVar (LowerVar "a")) (ExpListComp (ExpVar (LowerVar "b")) [Guard (ExpVar (LowerVar "c")), Gen (PatVar (LowerVar "d")) (ExpVar (LowerVar "e"))])
            , DefVar
                (PatVar (LowerVar "a"))
                ( ExpCase
                    (ExpVar (LowerVar "b"))
                    [ CaseCl (PatInt 1) [GuardCl Nothing (ExpInt 1)]
                    , CaseCl
                        (PatInt 1)
                        [ GuardCl (Just (ExpBool True)) (ExpInt 1)
                        , GuardCl (Just (ExpBool False)) (ExpInt 1)
                        ]
                    ]
                    [DefVar (PatVar (LowerVar "a")) (ExpInt 1)]
                )
            , DefFunc (LowerVar "f") [PatVar (LowerVar "a")] [GuardCl Nothing (ExpInt 1)]
            , DefFunc (LowerVar "f") [PatVar (LowerVar "a")] [GuardCl (Just (ExpBool True)) (ExpInt 1)]
            , DefVar (PatVar (LowerVar "a")) (ExpFuncApp (ExpVar (LowerVar "f")) (ExpInt 1))
            ]
        )
    runFlParserTest
        "\
        \ a = -1 + 1 - 1 * 1 / 1       \n\
        \ a = 1 == 1                   \n\
        \ a = 1 /= 1                   \n\
        \ a = 1 < 1                    \n\
        \ a = 1 <= 1                   \n\
        \ a = 1 > 1                    \n\
        \ a = 1 >= 1                   \n\
        \ a = True && not True || True \n\
        \ a = (1 : [] ++ []) !! 0      \n\
        \"
        ( Prog
            [ DefVar (PatVar (LowerVar "a")) (ExpInfix Minus (ExpInfix Plus (ExpPrefix Neg (ExpInt 1)) (ExpInt 1)) (ExpInfix Div (ExpInfix Mul (ExpInt 1) (ExpInt 1)) (ExpInt 1)))
            , DefVar (PatVar (LowerVar "a")) (ExpInfix Eq (ExpInt 1) (ExpInt 1))
            , DefVar (PatVar (LowerVar "a")) (ExpInfix NotEq (ExpInt 1) (ExpInt 1))
            , DefVar (PatVar (LowerVar "a")) (ExpInfix Ls (ExpInt 1) (ExpInt 1))
            , DefVar (PatVar (LowerVar "a")) (ExpInfix Lse (ExpInt 1) (ExpInt 1))
            , DefVar (PatVar (LowerVar "a")) (ExpInfix Gt (ExpInt 1) (ExpInt 1))
            , DefVar (PatVar (LowerVar "a")) (ExpInfix Gte (ExpInt 1) (ExpInt 1))
            , DefVar (PatVar (LowerVar "a")) (ExpInfix Or (ExpInfix And (ExpBool True) (ExpPrefix Not (ExpBool True))) (ExpBool True))
            , DefVar (PatVar (LowerVar "a")) (ExpInfix Index (ExpGroup (ExpInfix Cons (ExpInt 1) (ExpInfix Concat (ExpList []) (ExpList [])))) (ExpInt 0))
            ]
        )
    runFlParserTest
        "\
        \ a :: (a)                 \n\
        \ a :: a                   \n\
        \ a :: Int                 \n\
        \ a :: Char                \n\
        \ a :: Bool                \n\
        \ a :: String              \n\
        \ a :: [Int]               \n\
        \ a :: (Int, Int)          \n\
        \ a :: Int -> Int          \n\
        \ a :: (Int -> Int) -> Int \n\
        \ data T = A               \n\
        \ data T a = A a | B       \n\
        \"
        ( Prog
            [ DefType (LowerVar "a") (TyGroup (TyVar (LowerVar "a")))
            , DefType (LowerVar "a") (TyVar (LowerVar "a"))
            , DefType (LowerVar "a") TyInt
            , DefType (LowerVar "a") TyChar
            , DefType (LowerVar "a") TyBool
            , DefType (LowerVar "a") TyStr
            , DefType (LowerVar "a") (TyList TyInt)
            , DefType (LowerVar "a") (TyTuple [TyInt,TyInt])
            , DefType (LowerVar "a") (TyFunc [TyInt,TyInt])
            , DefType (LowerVar "a") (TyFunc [TyGroup (TyFunc [TyInt,TyInt]),TyInt])
            , DefAdt (UpperVar "T") [] [Ctor (UpperVar "A") []]
            , DefAdt (UpperVar "T") [TyVar (LowerVar "a")] [Ctor (UpperVar "A") [TyVar (LowerVar "a")],Ctor (UpperVar "B") []]
            ]
        )
