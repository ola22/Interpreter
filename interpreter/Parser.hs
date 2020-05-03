{- Parser was writen based on https://markkarpov.com/tutorial/megaparsec.html tutorial -}

{-# Options -Wall -Wname-shadowing #-}



module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.Expr as E
import qualified Data.Set as S
import Control.Monad

import Definitions
import BuiltinPrimiFuncs


type Parser = Parsec Void String







------------------------------------------------- READING ---------------------------------------------

-- Function reads following spaces and comments
readSpacesAndComments :: Parser ()
readSpacesAndComments = L.space
    space1
    (L.skipLineComment "//=^.^=")
    (L.skipBlockComment "/=^.^=" "=^.^=/")


lexeme :: Parser a -> Parser a
lexeme = L.lexeme readSpacesAndComments


symbol :: String -> Parser String
symbol = L.symbol readSpacesAndComments


-- Function reads single string 
readString :: String -> Parser ()
readString s = try $ string s
            *> notFollowedBy (alphaNumChar <|> char '_') 
            *> readSpacesAndComments


-- Function reads single operator
readOperator :: String -> Parser ()
readOperator s = try $ string s 
            *> notFollowedBy (oneOf "[]=!<>&|-+*/%\\;") 
            *> readSpacesAndComments


-- Function reads list
readListOp :: String -> Parser ()
readListOp s = try $ string s 
            *> readSpacesAndComments






--------------------------------------------- PARSING ------------------------------------------------------
-------------------------------- (creating parseTree for read elements) ------------------------------------

-- parseParens parses parenthesis
parseParens :: Parser a -> Parser a
parseParens = between (symbol "(") (symbol ")")


-- ParseNumber parses positive integers
parsePositiveNumber :: Parser ParseTree
parsePositiveNumber = do
    n <- lexeme L.decimal
    return (TData (DInt n))


-- ParseNegativeNumber parses negative integers
parseNegativeNumber :: Parser ParseTree
parseNegativeNumber = do
    readOperator "-"
    n <- lexeme L.decimal
    return (TData (DInt (0 - n)))


-- ParseNumber parses boolExpressions
parseBool :: Parser ParseTree
parseBool = do
    b <- (readString "false" *> pure False) <|>
         (readString "true" *> pure True)
    return (TData (DBool b))


-- Function parses list
-- [1, 2, 3]
parseList :: Parser ParseTree
parseList = do
    readListOp "["
    content <- sepBy (parseExprHelper <|> parseidentifier) (symbol ",")
    readListOp "]"
    return (TData (DList content))


-- parseIf parses if statement
parseIf :: Parser ParseTree
parseIf = do
    readString "if"
    b_exp <- parseExprHelper
    readString "then"
    e1 <- parseExprHelper
    readString "else"
    e2 <- parseExprHelper
    return (TFAppl (TFAppl (TFAppl 
            (TData (DPrimi primiIf)) b_exp) e1) e2)



-- parseEmpty parses empty functions
parseEmpty :: Parser ParseTree
parseEmpty = do
    readString "empty"
    return (TData (DListPrimi primiEmpty))


-- parseIsEmpty parses isEmpty functions
parseIsEmpty :: Parser ParseTree
parseIsEmpty = do
    readString "isEmpty"
    list <- parseExprHelper
    return (TFAppl (TData (DListPrimi primiIsEmpty)) list)


-- parseHead parses head functions
parseHead :: Parser ParseTree
parseHead = do
    readString "head"
    list <- parseExprHelper
    return (TFAppl (TData (DListPrimi primiHead)) list)


-- parseTail parses tail functions
parseTail :: Parser ParseTree
parseTail = do
    readString "tail"
    list <- parseExprHelper
    return (TFAppl (TData (DListPrimi primiTail)) list)


-- parseConcat parses concat functions
parseConcat :: Parser ParseTree
parseConcat = do
    readString "concat"
    list1 <- parseExprHelper
    list2 <- parseExprHelper
    return (TFAppl (TFAppl 
            (TData (DListPrimi primiConcat)) list1) list2)



-- parseVarAndFuncNames parses names of variables and
-- functions created by user. Argument
-- "purpose" tells if it is func or var name
parseVarAndFuncNames :: String -> Parser String
parseVarAndFuncNames purpose = do
    name <- lexeme $ (:) <$> lowerChar <*> many (alphaNumChar <|> char '_')
    if isReservedSyntax name
        then fail ("Word: " ++ name 
            ++ " is a reserved syntax in Olol and therefore cannot be used as " 
            ++ purpose ++ " name.")
        else return name


-- Function checks if any element from list appears
-- there more than once
checkIfNotRepeted :: [String] -> Parser ()
checkIfNotRepeted l = foldM_ (\x e ->
    if S.member e x
        then fail ("Repeated variable name: " ++ show e)
        else return $ S.insert e x) S.empty l


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
                    || s == "head"
                    || s == "tail"
                    || s == "empty"
                    || s == "concat"
                    || s == "isEmpty"


-- Function parses variable/function name
parseidentifier :: Parser ParseTree
parseidentifier = TVar <$> parseVarAndFuncNames "variable/function"


-- Function parses anonymous function
-- \x -> x + 2
parseLambda :: Parser ParseTree
parseLambda = do
    readOperator "\\"
    vars <- try (some (parseVarAndFuncNames "variable"))
    checkIfNotRepeted vars
    readOperator "->"
    body <- parseExprHelper
    return (foldr TFunc body vars)




-- Following functions are used for parsing arithmetical,
-- comparison and logical operator, as well as function application
parseTerm :: Parser ParseTree
parseTerm = choice
  [ parseParens parseExprHelper
  , parseList
  , parseTail
  , parseEmpty
  , parseHead
  , parseConcat
  , parseIsEmpty
  , parseLambda
  , parseIf
  , parseBool
  , parseidentifier
  , parsePositiveNumber
  , parseNegativeNumber
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
    checkIfNotRepeted vars
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




-- parseProgamm parses whole programm
parseProgramm :: Parser Programm
parseProgramm = between readSpacesAndComments eof 
                       (many (parseDef <|> parseFun <|> parseExpr))


