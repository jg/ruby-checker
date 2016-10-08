{-# LANGUAGE ExplicitForAll, ScopedTypeVariables #-}
module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

type Program = [Statement]

data Statement = Assign String AExpr
  | If BExpr Statement Statement
  | While BExpr Statement
  | Return
  | Require String
  | Method [AExpr] [Statement] deriving (Show, Eq)

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
            deriving (Show, Eq)

-- Relational operators:
data BBinOp = And | Or deriving (Show, Eq)

-- Now we define the types for arithmetic expressions:
data RBinOp = Greater | Less deriving (Show, Eq)

-- arithmetic operators:
data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
             deriving (Show, Eq)

-- arithmetic operations
data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
              deriving (Show, Eq)

data Arg = AExpr deriving (Show, Eq)



languageDef =
  emptyDef { Token.commentStart    = ""
           , Token.commentEnd      = ""
           , Token.commentLine     = "#"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "return"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     , "require"
                                     , "def"
                                     , "end"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                     , "<", ">", "and", "or", "not"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    -- parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
stringLiteral = Token.stringLiteral lexer
comma = Token.comma lexer

whileParser :: Parser Program
whileParser = whiteSpace >> program

program :: Parser Program
program = sequenceOfStatement

sequenceOfStatement = do
  list <- (sepBy1 statement semi)
  return $ list

statement :: Parser Statement
statement =   ifStatement
           <|> whileStatement
           <|> returnStatement
           <|> assignStatement
           <|> requireStatement
           <|> methodStatement

arg :: Parser AExpr
arg = aExpression

args :: Parser [AExpr]
args = do
  list <- (sepBy1 arg comma)
  return $ list

methodStatement :: Parser Statement
methodStatement = do
  reserved "def"
  ident <- identifier
  args <- parens args
  body <- sequenceOfStatement
  reserved "end"
  return $ Method args body


ifStatement :: Parser Statement
ifStatement =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2

whileStatement :: Parser Statement
whileStatement =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt

assignStatement :: Parser Statement
assignStatement =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

requireStatement :: Parser Statement = do
  reserved "require"
  fileName <- stringLiteral
  return $ Require fileName
  

returnStatement :: Parser Statement
returnStatement = reserved "return" >> return Return


aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
             , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft,
                Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2
 
relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)

parseString :: String -> Program
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Program
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
