{-# LANGUAGE GADTs #-}

module FlParser (parseFl) where

import Control.Monad.Combinators.Expr
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Void (Void)
import FlDef
import Text.Megaparsec hiding (parse)
import Text.Megaparsec qualified as M (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (IndentOpt (IndentMany, IndentSome), decimal, indentBlock)

parseFl :: String -> Either String Prog
parseFl src = first errorBundlePretty (M.parse (space *> flProg <* eof) "" src)

type Parser = Parsec Void String

flSeq :: Parser a -> String -> Parser [a]
flSeq parser sep = (:) <$> parser <*> many item <* hspace <?> "Seq"
  where
    item = string sep *> hspace *> parser

flGroupSeq :: Char -> Parser a -> String -> Char -> Parser [a]
flGroupSeq open parser sep close = start *> body <* end <?> "GroupSeq"
  where
    start = char open *> hspace
    body = flSeq parser sep <|> pure []
    end = char close <* hspace

flCtor :: Parser a -> Parser (Ctor a)
flCtor parser = Ctor <$> flUpperVar <*> many parser <?> "Ctor"

flKeyword :: Parser String
flKeyword =
    choice
        [ string "Int"
        , string "Char"
        , string "Bool"
        , string "String"
        , string "case"
        , string "of"
        , string "where"
        ]
        <* hspace
        <?> "Keyword"

flLowerVar :: Parser LowerVar
flLowerVar = notFollowedBy flKeyword >> LowerVar <$> ((:) <$> lowerChar <*> many alphaNumChar) <* hspace <?> "LowerVar"

flUpperVar :: Parser UpperVar
flUpperVar = notFollowedBy flKeyword >> UpperVar <$> ((:) <$> upperChar <*> many alphaNumChar) <* hspace <?> "UpperVar"

flInt :: Parser Int
flInt = decimal <* hspace <?> "Int"

flChar :: Parser Char
flChar = char '\'' *> (alphaNumChar <|> char ' ') <* char '\'' <* hspace <?> "Char"

flBool :: Parser Bool
flBool = (True <$ string "True" <|> False <$ string "False") <* hspace <?> "Bool"

flStr :: Parser String
flStr = char '"' *> many (alphaNumChar <|> char ' ') <* char '"' <* hspace <?> "Str"

flPat :: Parser Pat
flPat =
    choice
        [ PatVar <$> flLowerVar
        , PatInt <$> flInt
        , PatChar <$> flChar
        , PatBool <$> flBool
        , PatStr <$> flStr
        , PatList <$> flGroupSeq '[' flPat "," ']'
        , PatTuple <$> flGroupSeq '(' flPat "," ')'
        , PatCtor <$> flCtor flPat
        ]
        <?> "Pat"

flType :: Parser Type
flType = label "Type" $ do
    tys <- flSeq flType_ "->"
    if length tys == 1 then pure (head tys) else pure (TyFunc tys)

flType_ :: Parser Type
flType_ =
    choice
        [ try $ TyGroup <$ char '(' <* hspace <*> flType <* char ')' <* hspace
        , TyVar <$> flLowerVar
        , TyInt <$ string "Int" <* hspace
        , TyChar <$ string "Char" <* hspace
        , TyBool <$ string "Bool" <* hspace
        , TyStr <$ string "String" <* hspace
        , TyList <$ char '[' <* hspace <*> flType <* char ']' <* hspace
        , TyTuple <$> flGroupSeq '(' flType "," ')'
        ]
        <?> "Type_"

flExp :: Parser Exp
flExp = makeExprParser flExp_ flTable <?> "Exp"

flExp_ :: Parser Exp
flExp_ = label "Exp_" $ do
    subExp <- flExp__
    case subExp of
        ExpCase _ _ -> pure subExp
        _ -> do
            subExps <- many flExp__
            pure $ foldl' ExpFuncApp subExp subExps

flExp__ :: Parser Exp
flExp__ =
    choice
        [ try $ ExpGroup <$ char '(' <* hspace <*> flExp <* char ')' <* hspace
        , ExpVar <$> flLowerVar
        , ExpInt <$> flInt
        , ExpChar <$> flChar
        , ExpBool <$> flBool
        , ExpStr <$> flStr
        , try $ ExpList <$> flGroupSeq '[' flExp "," ']'
        , ExpTuple <$> flGroupSeq '(' flExp "," ')'
        , ExpAdt <$> flCtor flExp
        , ExpListComp <$ start <*> flExp <* sep <*> flSeq flQualCl "," <* end
        , lookAhead (string "case") *> flExpCase
        ]
        <?> "Exp__"
  where
    start = char '[' <* hspace
    sep = char '|' <* hspace
    end = char ']' <* hspace

flQualCl :: Parser QualCl
flQualCl =
    choice
        [ try $ Gen <$> flPat <* string "<-" <* hspace <*> flExp
        , Guard <$> flExp
        ]
        <?> "QualCl"

flExpCase :: Parser Exp
flExpCase = indentBlock space parser <?> "ExpCase"
  where
    parser = do
        value <- string "case" *> hspace *> flExp <* string "of" <* hspace
        pure $ IndentSome Nothing (return . ExpCase value) flCaseCl

flCaseCl :: Parser CaseCl
flCaseCl = indentBlock space parser <?> "CaseCl"
  where
    parser = do
        pat <- flPat
        guard <- flGuardCl "->"
        pure $ IndentMany Nothing (pure . CaseCl pat . (:) guard) (flGuardCl "->")

flGuardCl :: String -> Parser GuardCl
flGuardCl sep = GuardCl <$> guard <* string sep <* hspace <*> flExp <?> "GuardCl"
  where
    guard = optional $ char '|' *> hspace *> flExp

flTable :: [[Operator Parser Exp]]
flTable =
    [
        [ InfixL $ ExpInfix Index <$ op "!!"
        , Prefix $ ExpPrefix Not <$ op "not"
        ]
    ,
        [ InfixL $ ExpInfix Mul <$ op "*"
        , InfixL $ ExpInfix Div <$ notFollowedBy (string "/=") <* op "/"
        ]
    ,
        [ InfixL $ ExpInfix Plus <$ notFollowedBy (string "++") <* op "+"
        , InfixL $ ExpInfix Minus <$ notFollowedBy (string "->") <* op "-"
        , Prefix $ ExpPrefix Neg <$ char '-'
        ]
    ,
        [ InfixR $ ExpInfix Cons <$ op ":"
        , InfixR $ ExpInfix Concat <$ op "++"
        ]
    ,
        [ InfixN $ ExpInfix Eq <$ op "=="
        , InfixN $ ExpInfix NotEq <$ op "/="
        , InfixN $ ExpInfix Lse <$ op "<="
        , InfixN $ ExpInfix Ls <$ op "<"
        , InfixN $ ExpInfix Gte <$ op ">="
        , InfixN $ ExpInfix Gt <$ op ">"
        ]
    ,
        [ InfixR $ ExpInfix And <$ op "&&"
        ]
    ,
        [ InfixR $ ExpInfix Or <$ op "||"
        ]
    ]
  where
    op s = string s <* hspace

flDef :: Parser Def
flDef =
    choice
        [ DefAdt <$ string "data" <* hspace <*> flUpperVar <*> many flType <* char '=' <* hspace <*> flSeq (flCtor flType) "|"
        , try $ DefType <$> flLowerVar <* string "::" <* hspace <*> flType
        , try $ DefVar <$> flPat <* char '=' <* hspace <*> flExp
        , flDefFunc
        ]
        <* space
        <?> "Def"

flDefFunc :: Parser Def
flDefFunc = indentBlock space parser <*> flWhere <?> "DefFunc"
  where
    parser = do
        name <- flLowerVar
        args <- many flPat
        guard <- flGuardCl "="
        pure $ IndentMany Nothing (pure . DefFunc name args . (:) guard) (flGuardCl "=")

flWhere :: Parser [Def]
flWhere = indentBlock space parser <|> pure [] <?> "Where"
  where
    parser = do
        _ <- string "where" <* hspace
        pure $ IndentSome Nothing pure flDef

flProg :: Parser Prog
flProg = Prog <$> many flDef <?> "Prog"
