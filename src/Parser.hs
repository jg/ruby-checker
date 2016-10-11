{-# LANGUAGE ExplicitForAll, ScopedTypeVariables #-}
module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Program = Seq [Statement]
  | ClassDefinition String (Maybe String) Program Info deriving (Show, Eq)

data Info = Info {
  sourceName :: String
  , lineNumber :: Int
  , columnNumber :: Int
  } deriving (Show, Eq)

data Statement = Assign String AExpr Info
  | If BExpr Program Program Info
  | While BExpr Program Info
  | Return Info
  | Require String Info
  | Method [AExpr] Program Info deriving (Show, Eq)

data BExpr = BoolConst Bool
           | Not BExpr Info
           | BBinary BBinOp BExpr BExpr Info
           | RBinary RBinOp AExpr AExpr Info
            deriving (Show, Eq)

-- Relational operators:
data BBinOp = And | Or 
            deriving (Show, Eq)

-- Now we define the types for arithmetic expressions:
data RBinOp = Greater
            | Less
            deriving (Show, Eq)

-- arithmetic operators:
data AExpr = Var String 
           | IntConst Integer 
           | Neg AExpr Info
           | ABinary ABinOp AExpr AExpr Info
             deriving (Show, Eq)

-- arithmetic operations
data ABinOp = Add 
            | Subtract 
            | Multiply 
            | Divide 
              deriving (Show, Eq)

data Arg = AExpr Info
  deriving (Show, Eq)



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
                                     , "class"
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
program = sequenceOfStatement <|> classDefinition

getInfo :: SourcePos -> Info
getInfo sp =
  Info (Text.ParserCombinators.Parsec.sourceName sp) (sourceLine sp) (sourceColumn sp)

classDefinition :: Parser Program
classDefinition = do
  pos <- getPosition
  reserved "class"
  className <- identifier
  superClassName <- optionMaybe superClassName
  body <- sequenceOfStatement
  reserved "end"
  return $ ClassDefinition className superClassName body (getInfo pos)

superClassName :: Parser String
superClassName = do
  reservedOp "<"
  name <- identifier
  return name

sequenceOfStatement :: Parser Program
sequenceOfStatement = do
  list <- (sepBy1 statement (optional $ many newline))
  return $ Seq list

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
  list <- (sepBy arg comma)
  return $ list

methodStatement :: Parser Statement
methodStatement = do
  pos <- getPosition
  reserved "def"
  ident <- identifier
  args <- option [] $ parens args
  body <- sequenceOfStatement
  reserved "end"
  return $ Method args body (getInfo pos)


ifStatement :: Parser Statement
ifStatement =
  do
     pos <- getPosition
     reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- sequenceOfStatement
     reserved "else"
     stmt2 <- sequenceOfStatement
     return $ If cond stmt1 stmt2 (getInfo pos)

whileStatement :: Parser Statement
whileStatement = do
     pos <- getPosition
     reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- sequenceOfStatement
     reserved "end"
     return $ While cond stmt (getInfo pos)

assignStatement :: Parser Statement
assignStatement = do
     pos <- getPosition
     var  <- identifier
     reservedOp "="
     expr <- aExpression
     return $ Assign var expr (getInfo pos)

requireStatement :: Parser Statement = do
  pos <- getPosition
  reserved "require"
  fileName <- stringLiteral
  return $ Require fileName (getInfo pos)
  

returnStatement :: Parser Statement
returnStatement = do
  pos <- getPosition
  reserved "return"
  return $ Return (getInfo pos)

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [prefix "-" (Neg)]
             , [binary "*" (ABinary Multiply) AssocLeft]
             , [binary "/" (ABinary Divide) AssocLeft]
             , [binary "+" (ABinary Add) AssocLeft]
             , [binary "-" (ABinary Subtract) AssocLeft]
             ]

bOperators = [ [prefix "not" (Not)]
             , [binary "and" (BBinary And) AssocLeft]
             , [binary "or"  (BBinary Or) AssocLeft]
             ]


prefix name fun = Prefix (prefix' name fun)

prefix' :: String -> (a -> Info -> a) -> Parser (a -> a)
prefix' name f = do
  pos <- getPosition
  reservedOp name
  return $ \expr -> (f expr (getInfo pos))

binary  name fun assoc = Infix (binary' name fun) assoc
binary' ::
  String ->
  (a -> a -> Info -> a) ->
  Parser (a -> a -> a)
binary' name f = do
  pos <- getPosition
  reservedOp name
  return $ \aexpr1 aexpr2 -> (f aexpr1 aexpr2 (getInfo pos))

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression = do
     pos <- getPosition
     a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2 (getInfo pos)
 
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
