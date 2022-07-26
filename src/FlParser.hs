-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module FlParser (parseFl) where

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

flKeyword :: Parser String
flKeyword =
    choice
        [ string "case"
        , string "of"
        , string "where"
        ]

flSeq :: Parser a -> String -> Parser [a]
flSeq p s = ((:) <$> p) <*> many (sep *> p)
  where
    sep = hspace *> string s *> hspace

flGroupSeq :: Char -> Parser a -> Char -> Char -> Parser [a]
flGroupSeq s p c e =
    start
        *> choice
            [ [] <$ end
            , flSeq p [c] <* end
            ]
  where
    start = char s *> hspace
    end = hspace <* char e

flCtor :: Parser a -> Parser (Fl.Ctor a)
flCtor p = Fl.Ctor <$> flUpperVar <* hspace <*> many (hspace *> p)

flLowerVar :: Parser Fl.LowerVar
flLowerVar = notFollowedBy flKeyword >> Fl.LowerVar <$> ((:) <$> lowerChar <*> many alphaNumChar)

flUpperVar :: Parser Fl.UpperVar
flUpperVar = Fl.UpperVar <$> ((:) <$> upperChar <*> many alphaNumChar)

flInt :: Parser Int
flInt = L.decimal

flChar :: Parser Char
flChar = char '\'' *> (alphaNumChar <|> char ' ') <* char '\''

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
        <* hspace

flType :: Parser Fl.Type
flType = do
    tys <- flSeq flType_ "->"
    if length tys == 1 then pure (head tys) else pure (Fl.TyFunc tys)

flType_ :: Parser Fl.Type
flType_ =
    choice
        [ try $ Fl.TyGroup <$ char '(' <* hspace <*> flType <* hspace <* char ')'
        , Fl.TyVar <$> flLowerVar
        , Fl.TyInt <$ string "Int"
        , Fl.TyChar <$ string "Char"
        , Fl.TyBool <$ string "Bool"
        , Fl.TyStr <$ string "String"
        , Fl.TyList <$ char '[' <*> flType <* char ']'
        , Fl.TyTuple <$> flGroupSeq '(' flType ',' ')'
        ]
        <* hspace

flQualCl :: Parser Fl.QualCl
flQualCl =
    choice
        [ try $ Fl.Gen <$> flPat <* sep <*> flExp
        , Fl.Guard <$> flExp
        ]
  where
    sep = hspace <* string "<-" <* hspace

flExpCase :: Parser Fl.Exp
flExpCase = L.indentBlock space p
  where
    p = do
        e <-  string "case" *> hspace *> flExp <* hspace <* string "of" <* hspace
        return $ L.IndentMany Nothing (return . Fl.ExpCase e) (flCaseCl <* hspace)

flCaseCl :: Parser Fl.CaseCl
flCaseCl = L.indentBlock space p
  where
    p = do
        pat <- flPat <* hspace
        guard <- flGuardCl "->"
        return $ L.IndentMany Nothing (\gs -> return $ Fl.CaseCl pat (guard:gs)) (flGuardCl "->" <* hspace)

flGuardCl :: String -> Parser Fl.GuardCl
flGuardCl s = Fl.GuardCl <$> guard <* sep <*> flExp <* hspace
  where
    guard = optional $ char '|' *> hspace *> flExp
    sep = hspace <* string s <* hspace

flExp_ :: Parser Fl.Exp
flExp_ = do
    x : xs <- some (flExp__ <* hspace)
    return $ foldl' Fl.ExpFuncApp x xs

flExp__ :: Parser Fl.Exp
flExp__ =
    choice
        [ 
        try $ Fl.ExpGroup <$ char '(' <* hspace <*> flExp <* hspace <* char ')'
        , Fl.ExpVar <$> flLowerVar
        , Fl.ExpInt <$> flInt
        , Fl.ExpChar <$> flChar
        , Fl.ExpBool <$> flBool
        , Fl.ExpStr <$> flStr
        , try $ Fl.ExpList <$> flGroupSeq '[' flExp ',' ']'
        , Fl.ExpTuple <$> flGroupSeq '(' flExp ',' ')'
        , Fl.ExpAdt <$> flCtor flExp
        , Fl.ExpListComp <$ char '[' <* hspace <*> flExp <* hspace <* char '|' <* hspace <*> flSeq flQualCl "," <* hspace <* char ']'
        , lookAhead (string "case") *> flExpCase
        ]
        <* hspace

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
        , InfixL $ try $ Fl.ExpInfix Fl.Div <$ op "/" <* notFollowedBy (char '=')
        ]
    ,
        [ InfixL $ try $ Fl.ExpInfix Fl.Plus <$ op "+" <* notFollowedBy (char '+')
        , InfixL $ Fl.ExpInfix Fl.Minus <$ hspace <* notFollowedBy (string "->") <* char '-' <* hspace
        , Prefix $ Fl.ExpPrefix Fl.Neg <$ op "-"
        ]
    ,
        [ InfixR $ Fl.ExpInfix Fl.Cons <$ op ":"
        , InfixR $ Fl.ExpInfix Fl.Concat <$ op "++"
        ]
    ,
        [ InfixN $ Fl.ExpInfix Fl.Eq <$ op "=="
        , InfixN $ Fl.ExpInfix Fl.NotEq <$ op "/="
        , InfixN $ try $ Fl.ExpInfix Fl.Ls <$ op "<" <* notFollowedBy (char '=')
        , InfixN $ Fl.ExpInfix Fl.Lse <$ op "<="
        , InfixN $ try $ Fl.ExpInfix Fl.Gt <$ op ">"  <* notFollowedBy (char '=')
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
    op s = hspace <* string s <* hspace


flDefFunc :: Parser Fl.Def
flDefFunc = L.indentBlock space p <*> flWhere
  where
    p = do
        name <- flLowerVar 
        args <- many (hspace *> flPat)  <* hspace
        guard <- flGuardCl "="
        -- return $ L.IndentMany Nothing (\gs -> return $ Fl.DefFunc name args (guard:gs)) (flGuardCl "=" <* hspace)
        l <- L.indentLevel
        _ <- error $ show l
        return $ L.IndentMany Nothing (\gs -> return $ Fl.DefFunc name args (guard:gs)) (flGuardCl "=" <* hspace)

flWhere :: Parser [Fl.Def]
flWhere = (lookAhead (string "where") *> L.indentBlock space p) <|> pure []
  where
    p = do
      _ <- string "where" <* hspace
      return (L.IndentMany Nothing return (flDef <* hspace)) 

flDef :: Parser Fl.Def
flDef = 
    choice
        [ Fl.DefAdt <$ string "data" <* hspace <*> flUpperVar <* hspace <*> many (flType<* hspace) <* char '=' <* hspace <*> flSeq (flCtor flType) "|"
        , try $ Fl.DefType <$> flLowerVar <* hspace <* string "::" <* hspace <*> flType
        , try $ Fl.DefVar <$> flPat <* hspace <* char '=' <* hspace <*> flExp
        , flDefFunc
        ]
        <* space

flProg :: Parser Fl.Prog
flProg = Fl.Prog <$> many flDef <* hspace

parseFl :: String -> Either String Fl.Prog
parseFl src = first errorBundlePretty (M.parse (hspace *> flProg <* eof) "" src)
