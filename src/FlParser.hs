module FlParser (parse) where

import FlDef qualified as Fl

import Data.Bifunctor
import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec qualified as M (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

skipSpace :: Parser ()
skipSpace = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

flSeq :: Parser a -> String -> Parser [a]
flSeq p s = ((:) <$> p) <*> many (sep *> p)
  where
    sep = skipSpace *> string s *> skipSpace

flGroupSeq :: Char -> Parser a-> Char -> Char -> Parser [a]
flGroupSeq s p c e = start *> choice
        [[] <$ end
        ,flSeq p [c] <* end
        ]
  where
    start = char s *> skipSpace
    end = skipSpace <* char e

flCtor :: Parser a -> Parser (Fl.Ctor a)
flCtor p = Fl.Ctor <$> flUpperVar <*> many (skipSpace *> p)

flLowerVar :: Parser Fl.LowerVar
flLowerVar = Fl.LowerVar <$> ((:) <$> lowerChar <*> many alphaNumChar)

flUpperVar :: Parser Fl.UpperVar
flUpperVar = Fl.UpperVar <$> ((:) <$> upperChar <*> many alphaNumChar)

flInt :: Parser Int
flInt = L.signed skipSpace L.decimal

flChar :: Parser Char
flChar = char '\'' *> alphaNumChar <|> char ' ' <* char '\''

flBool :: Parser Bool
flBool = True <$ string "True" <|> False <$ string "False"

flStr :: Parser String
flStr = char '"' *> manyTill L.charLiteral (char '"')

flList :: Parser [Fl.Exp]
flList = flGroupSeq '[' flExp ','  ']' 

flTuple :: Parser [Fl.Exp]
flTuple = flGroupSeq '(' flExp ',' ')'

flListPat :: Parser [Fl.Pat]
flListPat = flGroupSeq '[' flPat ',' ']' 

flTuplePat :: Parser [Fl.Pat]
flTuplePat = flGroupSeq '(' flPat ',' ')'

flCtorPat :: Parser (Fl.Ctor Fl.Pat)
flCtorPat = flCtor flPat

flPat :: Parser Fl.Pat
flPat =
    choice
        [ Fl.PatVar <$> flLowerVar,
        Fl.PatInt <$> flInt,
     Fl.PatChar <$> flChar,
     Fl.PatBool <$> flBool,
     Fl.PatStr <$> flStr,
     Fl.PatList <$> flListPat,
     Fl.PatTuple <$> flTuplePat,
     Fl.PatCtor <$> flCtorPat
        ]

flType :: Parser Fl.Type
flType =
    choice
        [ Fl.TyVar <$> flLowerVar,
        Fl.TyInt <$ string "Int",
     Fl.TyChar <$ string "Char",
     Fl.TyBool <$ string "Bool",
     Fl.TyStr <$ string "String",
     Fl.TyList <$ char '[' <*> flType <* char ']',
     Fl.TyTuple <$> flGroupSeq '(' flType ',' ')',
     Fl.TyFunc <$> flSeq flType "->"
        ]

flExp :: Parser Fl.Exp
flExp =
    lexeme $
        choice
            [ Fl.ExpBool <$> flBool
            , Fl.ExpList <$> flList
            ]
            
-- term = parens expr <|> integer <?> "term"

-- table = [ [ prefix  "-"  negate
--           , prefix  "+"  id ]
--         , [ postfix "++" (+1) ]
--         , [ binary  "*"  (*)
--           , binary  "/"  div  ]
--         , [ binary  "+"  (+)
--           , binary  "-"  (-)  ] ]

-- expr = makeExprParser term table <?> "expression"

-- binary  name f = InfixL  (f <$ symbol name)
-- prefix  name f = Prefix  (f <$ symbol name)
-- postfix name f = Postfix (f <$ symbol name)



-- flDef ::Parser Fl.Def
-- flDef =

flProg :: Parser Fl.Prog
flProg = label "Prog" (Fl.Prog [] <$> flExp)

parse :: String -> Either String Fl.Prog
parse src = first errorBundlePretty (M.parse (between skipSpace eof flProg) "" src)
