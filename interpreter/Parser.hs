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


-- Function returns actual file name and line number
getFilePos :: Parser FilePosition
getFilePos = do
  p <- getSourcePos
  return (FilePosition
    (sourceName p)
    (fromIntegral $ unPos $ sourceLine p))





--------------------------------------------- PARSING ------------------------------------------------------
-------------------------------- (creating parseTree for read elements) ------------------------------------

-- parseParens parses parenthesis
parseParens :: Parser a -> Parser a
parseParens = between (symbol "(") (symbol ")")


-- ParseNumber parses positive integers
parsePositiveNumber :: Parser ParseTree
parsePositiveNumber = do
    pos <- getFilePos
    n <- lexeme L.decimal
    return (TData pos (DInt n))


-- ParseNegativeNumber parses negative integers
parseNegativeNumber :: Parser ParseTree
parseNegativeNumber = do
    pos <- getFilePos
    readOperator "-"
    n <- lexeme L.decimal
    return (TData pos (DInt (0 - n)))


-- ParseNumber parses boolExpressions
parseBool :: Parser ParseTree
parseBool = do
    pos <- getFilePos
    b <- (readString "false" *> pure False) <|>
         (readString "true" *> pure True)
    return (TData pos (DBool b))


-- Function parses list
-- [1, 2, 3]
parseList :: Parser ParseTree
parseList = do
    pos <- getFilePos
    readListOp "["
    content <- sepBy (parseExprHelper <|> parseidentifier) (symbol ",")
    readListOp "]"
    return (TData pos (DList content))


-- parseIf parses if statement
parseIf :: Parser ParseTree
parseIf = do
    pos <- getFilePos
    readString "if"
    b_exp <- parseExprHelper
    pos2 <- getFilePos
    readString "then"
    e1 <- parseExprHelper
    pos3 <- getFilePos
    readString "else"
    e2 <- parseExprHelper
    return (TFAppl pos3 (TFAppl pos2 (TFAppl pos 
            (TData pos (DPrimi primiIf)) b_exp) e1) e2)



-- parseEmpty parses empty functions
parseEmpty :: Parser ParseTree
parseEmpty = do
    pos <- getFilePos
    readString "empty"
    return (TData pos (DPrimi primiEmpty))


-- parseIsEmpty parses isEmpty functions
parseIsEmpty :: Parser ParseTree
parseIsEmpty = do
    pos <- getFilePos
    readString "isEmpty"
    list <- parseExprHelper
    return (TFAppl pos (TData pos (DPrimi primiIsEmpty)) list)


-- parseHead parses head functions
parseHead :: Parser ParseTree
parseHead = do
    pos <- getFilePos
    readString "head"
    list <- parseExprHelper
    return (TFAppl pos (TData pos (DPrimi primiHead)) list)


-- parseTail parses tail functions
parseTail :: Parser ParseTree
parseTail = do
    pos <- getFilePos
    readString "tail"
    list <- parseExprHelper
    return (TFAppl pos (TData pos (DPrimi primiTail)) list)


-- parseConcat parses concat functions
parseConcat :: Parser ParseTree
parseConcat = do
    pos <- getFilePos
    readString "concat"
    list1 <- parseExprHelper
    list2 <- parseExprHelper
    return (TFAppl pos (TFAppl pos
            (TData pos (DPrimi primiConcat)) list1) list2)



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
parseidentifier = do
    var <- parseVarAndFuncNames "variable/function"
    pos <- getFilePos
    return (TVar pos var)


-- Function parses anonymous function
-- \x -> x + 2
parseLambda :: Parser ParseTree
parseLambda = do
    pos <- getFilePos
    readOperator "\\"
    vars <- try (some (parseVarAndFuncNames "variable"))
    checkIfNotRepeted vars
    readOperator "->"
    body <- parseExprHelper
    return (foldr (TFunc pos) body vars)




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
operatorTable =
    [ [ binary ":" (\l r -> TFAppl (getPos l) l r)]
  , [ binary "*" (\l r -> TFAppl (getPos l) (TFAppl (getPos l) (TData (getPos l) $ DPrimi primiMul) l) r)
    , binary "/" (\l r -> TFAppl (getPos l) (TFAppl (getPos l) (TData (getPos l) $ DPrimi primiDiv) l) r)
    , binary "%" (\l r -> TFAppl (getPos l) (TFAppl (getPos l) (TData (getPos l) $ DPrimi primiMod) l) r)]
  , [ binary "+" (\l r -> TFAppl (getPos l) (TFAppl (getPos l) (TData (getPos l) $ DPrimi primiAdd) l) r)
    , binary "-" (\l r -> TFAppl (getPos l) (TFAppl (getPos l) (TData (getPos l) $ DPrimi primiSub) l) r)]
  , [ binary ">" (\l r -> TFAppl (getPos l) (TFAppl (getPos l) (TData (getPos l) $ DPrimi primiMore) l) r)
    , binary "<" (\l r -> TFAppl (getPos l) (TFAppl (getPos l) (TData (getPos l) $ DPrimi primiLess) l) r)
    , binary "==" (\l r -> TFAppl (getPos l) (TFAppl (getPos l) (TData (getPos l) $ DPrimi primiEq) l) r)
    , binary "!=" (\l r -> TFAppl (getPos l) (TFAppl (getPos l) (TData (getPos l) $ DPrimi primiNotEq) l) r)]
  , [ binary "&&" (\l r -> TFAppl (getPos l) (TFAppl (getPos l) (TData (getPos l) $ DPrimi primiAnd) l) r)
    , binary "||" (\l r -> TFAppl (getPos l) (TFAppl (getPos l) (TData (getPos l) $ DPrimi primiOr) l) r)]
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
    pos <- getFilePos
    readString "fun"
    pos2 <- getFilePos
    name <- parseVarAndFuncNames "function"
    vars <- try (many (parseVarAndFuncNames "variable"))
    checkIfNotRepeted vars
    readOperator "="
    body <- parseExprHelper
    readOperator ";"
    return (PEFunc pos name (foldr (TFunc pos2) body vars))


-- parseDef parses variable definition
-- def x = exp 
parseDef :: Parser ProgElem
parseDef = do
    pos <- getFilePos
    readString "def"
    var_name <- parseVarAndFuncNames "variable"
    readOperator "="
    expr <- parseExprHelper
    readOperator ";"
    return (PEDef pos var_name (foldr (TFunc pos) expr []))




-- parseProgamm parses whole programm
parseProgramm :: Parser Programm
parseProgramm = between readSpacesAndComments eof 
                       (many (parseDef <|> parseFun <|> parseExpr))


