-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module FlParser (parse) where

import FlDef qualified as Fl

import Control.Monad.Combinators.Expr
import Data.Bifunctor
import Data.Foldable
import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec qualified as M (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

flSeq :: Parser a -> String -> Parser [a]
flSeq p s = ((:) <$> p) <*> many (sep *> p)
  where
    sep = space *> string s *> space

flGroupSeq :: Char -> Parser a -> Char -> Char -> Parser [a]
flGroupSeq s p c e =
    start
        *> choice
            [ [] <$ end
            , flSeq p [c] <* end
            ]
  where
    start = char s *> space
    end = space <* char e

flCtor :: Parser a -> Parser (Fl.Ctor a)
flCtor p = Fl.Ctor <$> flUpperVar <*> many (space *> p)

flLowerVar :: Parser Fl.LowerVar
flLowerVar = Fl.LowerVar <$> ((:) <$> lowerChar <*> many alphaNumChar)

flUpperVar :: Parser Fl.UpperVar
flUpperVar = Fl.UpperVar <$> ((:) <$> upperChar <*> many alphaNumChar)

flInt :: Parser Int
flInt = L.decimal

flChar :: Parser Char
flChar = char '\'' *> alphaNumChar <|> char ' ' <* char '\''

flBool :: Parser Bool
flBool = True <$ string "True" <|> False <$ string "False"

flStr :: Parser String
flStr = char '"' *> manyTill L.charLiteral (char '"')

flPat :: Parser Fl.Pat
flPat =
    choice
        [ Fl.PatVar <$> flLowerVar
        , Fl.PatInt <$> flInt
        , Fl.PatChar <$> flChar
        , Fl.PatBool <$> flBool
        , Fl.PatStr <$> flStr
        , Fl.PatList <$> flGroupSeq '[' flPat ',' ']'
        , Fl.PatTuple <$> flGroupSeq '(' flPat ',' ')'
        , Fl.PatCtor <$> flCtor flPat
        ]
        <* space

flType :: Parser Fl.Type
flType =
    choice
        [ Fl.TyVar <$> flLowerVar
        , Fl.TyInt <$ string "Int"
        , Fl.TyChar <$ string "Char"
        , Fl.TyBool <$ string "Bool"
        , Fl.TyStr <$ string "String"
        , Fl.TyList <$ char '[' <*> flType <* char ']'
        , Fl.TyTuple <$> flGroupSeq '(' flType ',' ')'
        , Fl.TyFunc <$> flSeq flType "->"
        ]
        <* space

flQualCl :: Parser Fl.QualCl
flQualCl =
    choice
        [ Fl.Guard <$> flExp
        , Fl.Gen <$> flPat <* sep <*> flExp
        ]
  where
    sep = space <* string "<-" <* space

flCaseCl :: Parser Fl.CaseCl
flCaseCl =
    Fl.CaseCl
        <$> flPat
        <*> many (space *> flGuardCl "->")

flFuncCl :: Parser Fl.FuncCl
flFuncCl =
    Fl.FuncCl
        <$> many flPat
        <*> many (space *> flGuardCl "=")

flGuardCl :: String -> Parser Fl.GuardCl
flGuardCl s = Fl.GuardCl <$> guard <* sep <*> flExp
  where
    guard = optional $ char '|' *> flExp
    sep = space <* string s <* space

flExp_ :: Parser Fl.Exp
flExp_ = do
    x : xs <- some (space *> flExp__)
    return $ foldl' Fl.ExpFuncApp x xs

flExp__ :: Parser Fl.Exp
flExp__ =
    choice
        [ try $ Fl.ExpGroup <$ char '(' <* space <*> flExp <* space <* char ')'
        , Fl.ExpVar <$> flLowerVar
        , Fl.ExpInt <$> flInt
        , Fl.ExpChar <$> flChar
        , Fl.ExpBool <$> flBool
        , Fl.ExpStr <$> flStr
        , try $ Fl.ExpList <$> flGroupSeq '[' flExp ',' ']'
        , Fl.ExpTuple <$> flGroupSeq '(' flExp ',' ')'
        , Fl.ExpAdt <$> flCtor flExp
        , Fl.ExpListComp <$ char '[' <* space <*> flExp <* space <* char '|' <* space <*> flSeq flQualCl "," <* space <* char ']'
        , Fl.ExpCase <$ string "case" <* space <*> flExp <* space <* string "of" <* space <*> many flCaseCl <* space <* string "where" <* space <*> many flDef
        , Fl.ExpFunc <$> flLowerVar <*> many flFuncCl
        ]
        <* space

flExp :: Parser Fl.Exp
flExp = makeExprParser flExp_ table

table :: [[Operator Parser Fl.Exp]]
table =
    [
        [ InfixL $ Fl.ExpInfix Fl.Index <$ op "!!"
        , Prefix $ Fl.ExpPrefix Fl.Not <$ op "not"
        ]
    ,
        [ InfixL $ Fl.ExpInfix Fl.Mul <$ op "*"
        , InfixL $ Fl.ExpInfix Fl.Div <$ op "/"
        ]
    ,
        [ InfixL $ Fl.ExpInfix Fl.Plus <$ space <* char '+' <* space
        , InfixL $ Fl.ExpInfix Fl.Minus <$ space <* char '-' <* space
        , Prefix $ Fl.ExpPrefix Fl.Neg <$ op "-"
        ]
    ,
        [ InfixR $ Fl.ExpInfix Fl.Cons <$ op ":"
        , InfixR $ Fl.ExpInfix Fl.Concat <$ op "++"
        ]
    ,
        [ InfixN $ Fl.ExpInfix Fl.Eq <$ op "=="
        , InfixN $ Fl.ExpInfix Fl.NotEq <$ op "/="
        , InfixN $ Fl.ExpInfix Fl.Ls <$ op "<"
        , InfixN $ Fl.ExpInfix Fl.Lse <$ op "<="
        , InfixN $ Fl.ExpInfix Fl.Gt <$ op ">"
        , InfixN $ Fl.ExpInfix Fl.Gte <$ op ">="
        ]
    ,
        [ InfixR $ Fl.ExpInfix Fl.And <$ op "&&"
        ]
    ,
        [ InfixR $ Fl.ExpInfix Fl.Or <$ op "||"
        ]
    ]
  where
    op s = space <* string s <* space

flDef :: Parser Fl.Def
flDef =
    choice
        [ Fl.DefAdt <$> flUpperVar <*> many (space *> flType) <* space <* string "=" <* space <*> many (space *> flCtor flType)
        , try $ Fl.DefType <$> flLowerVar <* space <* string "::" <* space <*> flType
        , Fl.DefVar <$> flPat <* space <* string "=" <*> flExp
        ]
        <* space

flProg :: Parser Fl.Prog
flProg = Fl.Prog <$> many flDef <* space

parse :: String -> Either String Fl.Prog
parse src = first errorBundlePretty (M.parse (space *> flProg <* eof) "" src)
