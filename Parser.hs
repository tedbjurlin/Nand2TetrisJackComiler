module Parser where

import           Data.Void (Void)
import           Text.Megaparsec as P (Parsec, try, many, errorBundlePretty, choice, satisfy, between, MonadParsec (eof), sepBy1, sepBy, runParser, parse)
import           Text.Megaparsec.Char (string, space1, letterChar, char, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char ( isPrint )
import Control.Monad.Combinators.Expr (makeExprParser, Operator (InfixL))
import Types

type Parser = Parsec Void String

whitespace :: Parser ()
whitespace = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

reserved :: String -> Parser String
reserved = lexeme . P.try . string

integer :: Parser Integer
integer =  lexeme L.decimal

symbol :: String -> Parser String
symbol = L.symbol whitespace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

curlyBraces :: Parser a -> Parser a
curlyBraces = between (symbol "{") (symbol "}")

pKeyword :: Parser Keyword
pKeyword = choice
  [ KTrue <$ reserved "true"
  , KFalse <$ reserved "false"
  , KNull <$ reserved "null"
  , KThis <$ reserved "this" ]

pUnaryOp :: Parser UnaryOp
pUnaryOp = choice
    [ UNeg <$ symbol "-"
    , UNot <$ symbol "~" ]

pVType :: Parser VType
pVType = choice
    [ TBool <$ reserved "boolean"
    , TInt <$ reserved "int"
    , TChar <$ reserved "char"
    , TClass <$> pIdent ]

pIdent :: Parser Ident
pIdent = lexeme $ do
  a <- choice
    [ letterChar
    , char '_' ]
  rest <-  many $ choice
    [ letterChar
    , digitChar
    , char '_' ]
  return $ a:rest

pCType :: Parser CType
pCType = choice
    [ Void <$ reserved "void"
    , V <$> pVType ]

pSubroutineCall :: Parser SubroutineCall
pSubroutineCall = choice
    [ try (UnQualified <$> pIdent <*> parens pExpressionList)
    , Qualified <$> (pIdent <* symbol ".") <*> pIdent <*> parens pExpressionList ]

pExpressionList :: Parser [Expression]
pExpressionList = sepBy pExpression (symbol ",")

isValid :: Char -> Bool
isValid '\n' = False
isValid '"'  = False
isValid c    = isPrint c

stringChar :: Parser Char
stringChar = satisfy isValid

pStr :: Parser String
pStr = quotes $ many stringChar

pTerm :: Parser Expression
pTerm = choice
    [ Unary <$> pUnaryOp <*> pTerm
    , parens pExpression
    , IntegerConstant <$> integer
    , StringConstant <$>  pStr
    , KeywordConstant <$> pKeyword
    , try (TermSubroutine <$> pSubroutineCall)
    , try (Array <$> pIdent <*> brackets pExpression)
    , Variable <$> pIdent ]

pExpression :: Parser Expression
pExpression = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expression]]
operatorTable =
    [[ binary "+" (Binary Add)
    ,  binary "-" (Binary Sub)
    ,  binary "*" (Binary Mul)
    ,  binary "/" (Binary Div)
    ,  binary "&" (Binary And)
    ,  binary "|" (Binary Or)
    ,  binary "<" (Binary Lt)
    ,  binary ">" (Binary Gt)
    ,  binary "=" (Binary Eq) ]]

binary :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binary  name f = InfixL (f <$ symbol name)

pClassVar :: Parser [ClassVar]
pClassVar = choice
    [ reserved "static" *> pVType >>= (\m -> sepBy1 (CStatic m <$> pIdent) (symbol ","))
    , reserved "field"  *> pVType >>= (\m -> sepBy1 (CField m <$> pIdent) (symbol ",")) ] <* symbol ";"

pSVar :: Parser [SVar]
pSVar = reserved "var" *> (pVType >>= (\m -> sepBy1 (SVar m <$> pIdent) (symbol ","))) <* symbol ";"

pSubroutine :: Parser Subroutine
pSubroutine = choice
    [ Constructor <$ reserved "constructor"
    , Function    <$ reserved "function"
    , Method      <$ reserved "method" ]
    <*> pCType <*> pIdent <*> parens (sepBy pParameter (symbol ",")) <*> pBody

pBody :: Parser Body
pBody = curlyBraces (B <$> (concat <$> many pSVar) <*> many pStatement)

pParameter :: Parser Parameter
pParameter = P <$> pVType <*> pIdent

pStatement :: Parser Statement
pStatement = choice
    [ try pLetArray
    , pLet
    , try pIfElse
    , pIf
    , pWhile
    , pDo
    , try pReturn
    , pReturnVoid ]

pLet :: Parser Statement
pLet = Let <$> (reserved "let" *> pIdent) <*> (symbol "=" *> pExpression <* symbol ";")

pLetArray :: Parser Statement
pLetArray = LetArray
    <$> (reserved "let" *> pIdent)
    <*> brackets pExpression
    <*> (symbol "=" *> pExpression <* symbol ";")

pIfElse :: Parser Statement
pIfElse = IfElse
    <$> (reserved "if" *> parens pExpression)
    <*> curlyBraces (many pStatement)
    <*> (reserved "else" *> curlyBraces (many pStatement))

pIf :: Parser Statement
pIf = If
    <$> (reserved "if" *> parens pExpression)
    <*> curlyBraces (many pStatement)

pWhile :: Parser Statement
pWhile = While
    <$> (reserved "while" *> parens pExpression) <*> curlyBraces (many pStatement)

pDo :: Parser Statement
pDo = Do <$> (reserved "do" *> pSubroutineCall <* symbol ";")

pReturn :: Parser Statement
pReturn = Return <$> (reserved "return" *> pExpression <* symbol ";")

pReturnVoid :: Parser Statement
pReturnVoid = ReturnV <$ reserved "return" <* symbol ";"

pClass :: Parser Class
pClass = C <$> (reserved "class" *> pIdent) <*> pClassBody

pClassBody :: Parser ClassBody
pClassBody = curlyBraces (CBody <$> (concat <$> many pClassVar) <*> many pSubroutine)

pJack :: Parser Class
pJack = whitespace *> pClass <* eof

parseJack :: String -> Either String Class
parseJack s = case parse pJack "" s of
    Left err -> Left (errorBundlePretty err)
    Right c  -> Right c


test :: Show a => Parser a -> String -> String
test p s = case runParser p "" s of
    Left err -> errorBundlePretty err
    Right c -> show c