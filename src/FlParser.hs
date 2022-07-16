module FlParser (parseSExp) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

newtype Identifier = Identifier
    { getId :: String
    }
    deriving (Show)

data SExp
    = SSExp SExp [SExp] -- (foo 0 "hello" bar), (bar (baz 1)), (foo)
    | SInteger Integer -- 42
    | SString String -- "hello, world"
    | SBool Bool -- false, true
    | SId Identifier -- foo
    deriving (Show)

type Parser = Parsec Void String

bool :: Parser Bool
bool = False <$ string "false" <|> True <$ string "true"

integer :: Parser Integer
integer = label "integer" $ L.signed skipSpace L.decimal

-- charLiteral also takes escaped characters into question.
str :: Parser String
str = label "string" $ char '"' *> manyTill L.charLiteral (char '"')

identifier :: Parser Identifier
identifier = label "identifier" $ do
    first <- letterChar <|> char '_'
    rest <- many $ alphaNumChar <|> char '_'
    pure $ Identifier $ first : rest

-- identifie :: Parser Identifier
-- identifie = Identifier <$> label "identifier" ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))

sexp :: Parser (SExp, [SExp])
sexp = label "S-expression" $ lexeme $
  between (lexeme (char '(')) (char ')') ((,) <$> atom <*> many atom)

atom :: Parser SExp
atom =
    lexeme $
        choice
            [ SBool <$> bool
            , SInteger <$> integer
            , SString <$> str
            , SId <$> identifier,
            uncurry SSExp <$> sexp
            ]

skipSpace :: Parser ()
skipSpace =
    L.space
        -- Like `space`, but skips 1 or more space characters.
        space1
        -- Skip from ;; until a newline.
        (L.skipLineComment ";;")
        -- Skip from /* until */. There is also `skipBlockComment`, but it doesn't handle nested comments.
        (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

parseSExp :: String -> Either String SExp
parseSExp input =
    let outputE =
            parse
                -- Skip any whitespace at the beginning, expect the end of input after the atom.
                (between skipSpace eof atom)
                -- Name of the source file, you can give it any name you want. I leave it blank for now.
                ""
                -- The actual string to be parsed
                input
     in -- If we get Left, it will be an `ParseErrorBundle`, let's pretty print it for now.
        -- If we get Right, do nothing.
        case outputE of
            Left err -> Left $ errorBundlePretty err
            Right output -> Right output