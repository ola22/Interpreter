
{- Parser was writen based on https://markkarpov.com/tutorial/megaparsec.html tutorial -}


module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.Expr as E

import Definitions
import TreeEvaluator


type Parser = Parsec Void String





------------------------------------------------- READING ---------------------------------------------

-- Function reads following spaces and comments
readSpacesAndComments :: Parser ()
readSpacesAndComments = L.space
    space1
    (L.skipLineComment "//=^.^=")
    (L.skipBlockComment "/=^.^=" "=^.^=\\")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme readSpacesAndComments


symbol :: String -> Parser String
symbol = L.symbol readSpacesAndComments


-- Function reads single string 
readString :: String -> Parser ()
readString s = try $ string s                                        ---- TO ZMIENIC!!!!!!!!!!!!!!!
            *> notFollowedBy (alphaNumChar <|> char '_') 
            *> readSpacesAndComments

-- Function reads single operator
readOperator :: String -> Parser ()
readOperator s = try $ string s 
            *> notFollowedBy (oneOf "=!<>&|-+*/%\\;") 
            *> readSpacesAndComments






--------------------------------------------- PARSING ------------------------------------------------------
-------------------------------- (creating parseTree for read elements) ------------------------------------

-- parseParens parses parenthesis
parseParens :: Parser a -> Parser a
parseParens = between (symbol "(") (symbol ")")


-- ParseNumber parses integers
-- (negative ones are not allowed)
parseNumber :: Parser ParseTree
parseNumber = do
    n <- lexeme L.decimal
    return (TData (DInt n))
{- | pInt
>>> parseTest parseNumber "376"
TData DInt 376
-}


-- ParseNumber parses boolExpressions
parseBool :: Parser ParseTree
parseBool = do
    b <- (readString "false" *> pure False) <|>
         (readString "true" *> pure True)
    return (TData (DBool b))
{- | pInt
>>> parseTest parseBool "false"
TData DBool False
-}


-- parseIf parses if statement
parseIf :: Parser ParseTree
parseIf = do
    readString "if"
    b_exp <- parseExprHelper
    readString "then"
    e1 <- parseExprHelper
    readString "else"
    e2 <- parseExprHelper
    return (TFAppl (TFAppl (TFAppl (TData (DPrimi primiIf)) b_exp) e1) e2)


-- parseVarAndFuncNames parses names of variables and
-- functions created by user. Argument
-- "purpose" tells if it is func or var name
parseVarAndFuncNames :: String -> Parser String
parseVarAndFuncNames purpose = do
    name <- lexeme $ (:) <$> lowerChar <*> many alphaNumChar
    if isReservedSyntax name
        then fail ("Word: " ++ name 
            ++ " is a reserved syntax in Olol and therefore cannot be used as " 
            ++ purpose ++ " name.")
        else return name

-- Function checks if a name chosen by user is not
-- an Olol keyword
isReservedSyntax :: String -> Bool
isReservedSyntax s = s == "if"
                    || s == "then"
                    || s == "else"
                    || s == "fun"
                    || s == "def"
                    || s == "true"
                    || s == "false"


-- Function parses variable/function name
parseidentifier :: Parser ParseTree
parseidentifier = TVar <$> parseVarAndFuncNames "variable/function"


--parseIdentifier :: Parser ParseTree


-- Function parses anonymous function
-- \x -> x + 2
parseLambda :: Parser ParseTree
parseLambda = do
    readOperator "\\"
    vars <- try (many (parseVarAndFuncNames "variable"))
    readOperator "->"
    body <- parseExprHelper
    readOperator ";"
    return (foldr TFunc body vars)


-- Following functions are used for parsing arithmetical,
-- comparison and logical operator, as well as function application
parseTerm :: Parser ParseTree
parseTerm = choice
  [ parseParens parseExprHelper
  , parseNumber
  , parseBool
  , parseIf
  , parseidentifier
  --, parseIdentifier
  , parseLambda
  ]


operatorTable :: [[E.Operator Parser ParseTree]]
operatorTable = [ [ binary ":" (\l r -> TFAppl l r)]
  , [ binary "*" (\l r -> TFAppl (TFAppl (TData $ DPrimi primiMul) l) r)
    , binary "/" (\l r -> TFAppl (TFAppl (TData $ DPrimi primiDiv) l) r)
    , binary "%" (\l r -> TFAppl (TFAppl (TData $ DPrimi primiMod) l) r)]
  , [ binary "+" (\l r -> TFAppl (TFAppl (TData $ DPrimi primiAdd) l) r)
    , binary "-" (\l r -> TFAppl (TFAppl (TData $ DPrimi primiSub) l) r)]
  , [ binary ">" (\l r -> TFAppl (TFAppl (TData $ DPrimi primiMore) l) r)
    , binary "<" (\l r -> TFAppl (TFAppl (TData $ DPrimi primiLess) l) r)
    , binary "==" (\l r -> TFAppl (TFAppl (TData $ DPrimi primiEq) l) r)
    , binary "!=" (\l r -> TFAppl (TFAppl (TData $ DPrimi primiNotEq) l) r)]
  , [ binary "&&" (\l r -> TFAppl (TFAppl (TData $ DPrimi primiAnd) l) r)
    , binary "||" (\l r -> TFAppl (TFAppl (TData $ DPrimi primiOr) l) r)]
  ]

binary :: String -> (ParseTree -> ParseTree -> ParseTree) -> E.Operator Parser ParseTree
binary  name f = E.InfixL  (f <$ symbol name)

prefix, postfix :: String -> (ParseTree -> ParseTree) -> E.Operator Parser ParseTree
prefix  name f = E.Prefix  (f <$ symbol name)
postfix name f = E.Postfix (f <$ symbol name)


-- Parsing expressions
parseExprHelper :: Parser ParseTree
parseExprHelper = E.makeExprParser parseTerm operatorTable


-- parsing any element of type PEExpr
parseExpr :: Parser ProgElem
parseExpr = do
    expr <- parseExprHelper
    readOperator ";"
    return (PEExpr expr)


-- parseFun parses function definition
-- fun f x .. = exp
parseFun :: Parser ProgElem
parseFun = do
    readString "fun"
    name <- parseVarAndFuncNames "function"
    vars <- try (many (parseVarAndFuncNames "variable"))
    readOperator "="
    body <- parseExprHelper
    readOperator ";"
    return (PEFunc name (foldr TFunc body vars))


-- parseDef parses variable definition
-- def x = exp 
parseDef :: Parser ProgElem
parseDef = do
    readString "def"
    var_name <- parseVarAndFuncNames "variable"
    readOperator "="
    expr <- parseExprHelper
    readOperator ";"
    return (PEDef var_name (foldr TFunc expr []))
    -- return (PEDef var_name (TFunc var_name expr))                               --- TU POWAZNA ZMIENA
{- | Pase programm elements
>>> parseTest parseDef "def x = 3;"
PEDef "x" (TData DInt 3)
>>> parseTest parseFun "fun f x y = x + y; "
PEFunc "f" (TFunc "x" (TFunc "y" (TFAppl (TFAppl (TData DPrimi 2) (TVar "x")) (TVar "y"))))
-}


-- parseProgamm parses whole programm
parseProgramm :: Parser [ProgElem]
parseProgramm = between readSpacesAndComments eof 
                       (many (parseDef <|> parseFun <|>parseExpr))
{- | Parse programm test
>>> parseTest parseProgramm "fun f x y = x + y; f : 1 : 2;"
[PEFunc "f" (TFunc "x" (TFunc "y" (TFAppl (TFAppl (TData DPrimi 2) (TVar "x")) (TVar "y")))),PEExpr (TFAppl (TFAppl (TVar "f") (TData DInt 1)) (TData DInt 2))]
-}

