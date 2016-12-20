{-# LANGUAGE OverloadedStrings #-}
module Lang.Huginn.Parser where
              -- ( readExpr
              -- , parseExpr
              -- , ParseError
              -- , testParser
              -- ) where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (haskellDef)
import qualified  Text.ParserCombinators.Parsec.Token as P
import Data.Semigroup ((<>))

import Control.Applicative
import Lang.Huginn.AST
import Lang.Huginn.Eval (EnvEntry)

-- PARSER
mkFnRec name fn = (name, parseExpr fn)

mkRow i name value value_type = (i, name, value, value_type)
mkInstanceRow i name value value_type = (i, readExpr name value value_type)
-- mkTestRow i name "Error" tvalue = (i, name, 0.01010101, tvalue, False)
mkTestRow i name hvalue tvalue = (i, name, hv, tv, round hv == round tv)
    where hv = read hvalue :: Double
          tv = read tvalue :: Double

-- TSV
notTabDelim = many (noneOf "\t\n")

tsvCell = stringLiteral <|> notTabDelim
tsvToken = tab *> tsvCell

tsvFunction = mkFnRec <$> tsvCell <*> tsvToken
tsvFunctionsFile = tsvFunction `endBy` string "\n" <* eof
parseTSVFunctions = parse tsvFunctionsFile "ERROR"

tsvInstanceFile = tsvInstanceLine `endBy` string "\n" <* eof
tsvInstanceLine = mkInstanceRow <$> natural <*> tsvCell <*> tsvToken <*> tsvToken
parseTSVInstances = parse tsvInstanceFile "ERROR"

tsvTestValuesFile = tsvTestValuesLine `endBy` string "\n" <* eof
tsvTestValuesLine = mkTestRow <$> natural <*> tsvCell <*> tsvToken <*> tsvToken
parseTSVTestValues = parse tsvTestValuesFile "ERROR"

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef { P.reservedOpNames = ["==", "=", "^", "+", "-", "*", "/"]})

lexeme = P.lexeme lexer

ws = P.whiteSpace lexer
comma = P.comma lexer
natural = P.natural lexer

closure :: Parser Expr
-- closure =  Closure <$> (lexeme (char '(') *> code <* lexeme (char ')'))
closure = Closure <$> parens code

ifStatement :: Parser Expr
ifStatement = do
  _ <- lexeme (string "if")
  test <- lexeme expr
  _ <- lexeme (string "then")
  left <- expr
  -- _ <- lexeme (string "else")
  -- right <- expr
  right <- elseStatement
  return $ If (test, left, right)

pythonIf :: Parser Expr
pythonIf = do
  left <- exprPython
  _ <- lexeme (string "if")
  test <- expr
  -- _ <- lexeme (string "else")
  -- right <- expr
  right <- elseStatement
  return $ If (test, left, right)

elseStatement = lexeme (string "else") *> expr

stxVar :: Parser Expr
stxVar = Var <$> var

word :: Parser String
word = lexeme $ many (noneOf " +-?<>\t\n,;()[]{}*^%&$#@!/|\'\"")

line :: Parser String
line = many (noneOf "\n")

file :: Parser [String]
file = line `sepBy` string "\n" <* eof

bool :: Parser Bool
bool = string "true" *> pure True <|> string "false" *> pure False

var :: Parser String
-- var = P.identifier lexer
var = do
  v1 <- letter
  v2 <- replace '.' '_' <$> word
  return $ v1:v2

replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace o n (x:xs) = if x == o then
                       n:replace o n xs
                     else
                       x:replace o n xs

brackets = P.brackets lexer

commaSep = P.commaSep lexer

array :: Parser [Expr]
array = brackets $ commaSep expr
-- array = lexeme (char '[') *> expr `sepBy` lexeme (char ',') <* lexeme (char ']')

stxArray :: Parser Expr
stxArray = Arr <$> array

stxBool :: Parser Expr
stxBool = Bl <$> lexeme bool

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer
  -- char '"' *> many (noneOf "\"") <* char '"'

stxString :: Parser Expr
stxString = Str <$> stringLiteral

stxLet :: Parser Expr
stxLet = do
  _ <- lexeme (string "let")
  ident <- word
  _ <- lexeme (string "=")
  val <- code
  _ <- lexeme (string "in")
  exp <-lexeme code

  return $ Let ident val exp


-- TODO: check for reserved words in variable names e.g. "if"
expr :: Parser Expr
expr = try bla <|> stxString <|> stxArray <|> closure <|> stxNum <|> stxBool <|> stxLet  <|> stxVar

parens = P.parens lexer

double = P.float lexer

integer = P.integer lexer

todouble x = fromInteger x :: Double

readDouble x = read x :: Double

noZero :: Parser String
noZero = do
  _ <- char '.'
  int <- many1 digit
  return ("0." <> int)

-- number = todouble <$> P.naturalOrFloat lexer
--  noZero parser fixes faulty input e.g ".34"
number = try double <|> todouble <$> integer <|> readDouble <$> noZero

stxNum = Num <$> number

reservedOp = P.reservedOp lexer

binary name fn =  Infix (do {reservedOp  name; return fn})

table = [
             [ binary "^" Epow AssocLeft]
             ,[ binary "*" Emul AssocLeft, binary "/" Ediv AssocLeft]
             ,[ binary "+" Eadd AssocLeft, binary "-" Esub AssocLeft]
             ,[ binary "<" Elt AssocLeft]
             ,[ binary "<=" Elte AssocLeft]
             ,[ binary ">" Egt AssocLeft]
             ,[ binary ">=" Egte AssocLeft]
             -- ,[ binary "=" Eeq AssocLeft]
             ,[ binary "==" Eeq AssocLeft]
          ]

term :: Parser Expr
term = parens bla <|> stxNum <|> stxString <|> stxBool <|> stxLet <|> stxVar

bla :: Parser Expr
bla = buildExpressionParser table term <?> "expression"

exprPython :: Parser Expr
exprPython = stxString <|> stxArray <|> closure <|> stxNum <|> (try stxBool <|> stxVar)

statement :: Parser Expr
statement = ifStatement <|> pythonIf

code :: Parser Expr
code = try statement <|> expr

formula :: Parser Expr
formula = code <* eof

-- No eval here, just read the value and parse it
-- Use this to create initial environment vars
readExpr :: String -> String -> String -> EnvEntry
readExpr n value "float" = (n, val)
  where val = case parse stxNum "NaN" value  of
                   Right x -> x
                   Left _ -> Err NaN
readExpr n value "bool" = (n, Bl (read value :: Bool))
readExpr n value _ = (n, Str value)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse formula "Parse error"

testParser :: Parser Expr -> String -> Either ParseError Expr
testParser p = parse p "Some Error"

