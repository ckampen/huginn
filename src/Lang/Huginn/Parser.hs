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

countParseResult (_,_,Left _) (lefts, rights) = (lefts + 1, rights)
countParseResult (_,_,Right _) (lefts, rights) = (lefts, rights + 1)


partitionFormulas (lefts, rights) a@(_,_,Left _) = (a:lefts, rights)
partitionFormulas (lefts, rights) a@(_,_,Right _) = (lefts, a:rights)

-- PARSER
-- mkFnRec ::
mkFnRec sheet name fn = (sheet, name, parseExpr fn)
-- mkFnRec sheet name fn = (sheet, name, fn)

-- mkRow i name value value_type = (i, name, value, value_type)
-- mkInstanceRow i name value value_type = (i, readExpr name value value_type)
-- -- mkTestRow i name "Error" tvalue = (i, name, 0.01010101, tvalue, False)
-- mkTestRow i name hvalue tvalue = (i, name, hv, tv, round hv == round tv)
--     where hv = read hvalue :: Double
--           tv = read tvalue :: Double

-- -- TSV
notTabDelim = many (noneOf "\t\n")

tsvCell = stringLiteral <|> notTabDelim
tsvToken = tab *> tsvCell

tsvFunction = mkFnRec <$> tsvCell <*> tsvToken <*> tsvToken
tsvFunctionsFile = tsvFunction `endBy` string "\n" <* eof
parseTSVFunctions = parse tsvFunctionsFile "ERROR"

-- tsvInstanceFile = tsvInstanceLine `endBy` string "\n" <* eof
-- tsvInstanceLine = mkInstanceRow <$> natural <*> tsvCell <*> tsvToken <*> tsvToken
-- parseTSVInstances = parse tsvInstanceFile "ERROR"

-- tsvTestValuesFile = tsvTestValuesLine `endBy` string "\n" <* eof
-- tsvTestValuesLine = mkTestRow <$> natural <*> tsvCell <*> tsvToken <*> tsvToken
-- parseTSVTestValues = parse tsvTestValuesFile "ERROR"

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef { P.reservedOpNames = ["==", "=", "^", "+", "-", "*", "/"]})

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

-- ws :: Parsec String () ()
ws :: Parser ()
ws = P.whiteSpace lexer
-- comma :: Parsec String () String
comma :: Parser String
comma = P.comma lexer
-- natural :: Parsec String () Integer
natural :: Parser Integer
natural = P.natural lexer

closure :: Parser Expr
-- closure =  Closure <$> (lexeme (char '(') *> code <* lexeme (char ')'))
closure = Closure <$> parens code

ifexcel :: Parser Expr
ifexcel = (string "IF") *> parens ifexcel'

ifexcel' :: Parser Expr
ifexcel' = do
  test <- expr
  _ <- lexeme (char ',')
  left <- code
  _ <- lexeme (char ',')
  right <- code
  return $ If (test, left, right)

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

excelIf :: Parser Expr
excelIf = do
  _ <- lexeme (string "IF")
  _ <- lexeme (char '(')
  test <- expr
  _ <- lexeme (char ',')
  left <- code
  _ <- lexeme (char ',')
  right <- code
  _ <- lexeme (char ')')
  return $ If (test, left, right)

countIf :: Parser Expr
countIf = do
  _ <- lexeme (string "COUNTIF")
  _ <- lexeme (char '(')
  range <- word
  _ <- lexeme (char ',')
  test <- stringLiteral
  _ <- lexeme (char ')')
  return $ CountIf range test

elseStatement :: Parser Expr
elseStatement = lexeme (string "else") *> expr

stxVar :: Parser Expr
stxVar = Var <$> var

word :: Parser String
word = lexeme $ many (noneOf " =+-?<>\t\n,;()[]{}*^%&$#@!/|\'\"")

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

-- brackets :: Parser Expr
brackets :: Parser a -> Parser a
brackets = P.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep lexer

array :: Parser [Expr]
array = brackets $ commaSep expr
-- array = lexeme (char '[') *> expr `sepBy` lexeme (char ',') <* lexeme (char ']')

concatenate :: Parser [Expr]
concatenate =  string "CONCATENATE" *> parens (commaSep code)
  -- return $ Concat elems
  --
stxConcat :: Parser Expr
stxConcat = Concat <$> concatenate


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

equals :: Parser Expr
equals = Eeq <$> expr <* (lexeme $ string "=") *> expr

-- TODO: check for reserved words in variable names e.g. "if"
expr :: Parser Expr
expr = try bla <|> stxString <|> stxArray <|> closure <|> stxNum <|> try stxBool <|> try stxLet <|> try statement <|> stxVar

-- parens :: Parser Expr -> Parser Expr
parens :: Parser a -> Parser a
parens = P.parens lexer

double :: Parser Double
double = P.float lexer

integer :: Parser Integer
integer = P.integer lexer

todouble :: Integer -> Double
todouble x = fromInteger x :: Double

readDouble :: String -> Double
readDouble x = read x :: Double

noZero :: Parser String
noZero = do
  _ <- char '.'
  int <- many1 digit
  return ("0." <> int)

-- number = todouble <$> P.naturalOrFloat lexer
--  noZero parser fixes faulty input e.g ".34"
number :: Parser Double
number = try double <|> todouble <$> integer <|> readDouble <$> noZero

stxNum :: Parser Expr
stxNum = Num <$> number

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

binary :: String -> (a -> a -> a) -> Assoc -> Operator Char () a
binary name fn =  Infix (do {reservedOp  name; return fn})

table :: [[Operator Char () Expr]]
table = [
             [ binary "^" Epow AssocLeft]
             ,[ binary "*" Emul AssocLeft, binary "/" Ediv AssocLeft]
             ,[ binary "+" Eadd AssocLeft, binary "-" Esub AssocLeft]
             ,[ binary "<" Elt AssocLeft]
             ,[ binary "<=" Elte AssocLeft]
             ,[ binary ">" Egt AssocLeft]
             ,[ binary ">=" Egte AssocLeft]
             ,[ binary "=" Eeq AssocLeft]
             ,[ binary "==" Eeq AssocLeft]
          ]

term :: Parser Expr
term = parens bla <|> stxNum <|> stxString <|> stxBool <|> stxLet <|> ifexcel <|> closure <|> stxVar

bla :: Parser Expr
bla = buildExpressionParser table (lexeme term) <?> "expression"

exprPython :: Parser Expr
exprPython = stxString <|> stxArray <|> closure <|> stxNum <|> (try stxBool <|> stxVar)

statement :: Parser Expr
statement = excelIf <|> ifStatement <|> try stxConcat <|> countIf <|> pythonIf

code :: Parser Expr
code = try statement <|> expr <|> closure

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

testString = "IF(DC105=\"\",0,DC105-(IF(DC195=\"\",0,DC195)+IF(DC285=\"\",0,DC285)+IF(DC375=\"\",0,DC375)+IF(DC465=\"\",0,DC465)+IF(DC555=\"\",0,DC555)+IF(DC645=\"\",0,DC645)+IF(DC735=\"\",0,DC735)+IF(DC825=\"\",0,DC825)+IF(DC915=\"\",0,DC915)+IF(DC1005=\"\",0,DC1005)+IF(DC1095=\"\",0,DC1095)+IF(DC1185=\"\",0,DC1185)+IF(DC1275=\"\",0,DC1275)+IF(DC1365=\"\",0,DC1365)+IF(DC1455=\"\",0,DC1455)+IF(DC1545=\"\",0,DC1545)))"
