module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Char Char
             | Bool Bool
             deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ args!!0

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err ->  "No match: " ++ show err
  Right val -> "Found value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseNumber 'd'
            <|> parseString
            <|> parseHash
            <|> parseAtom
            <|> parseQuoted
            <|> parseList 
                

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ choice [ noneOf ['\\', '"']
                     , char '\\' >>
                       choice [ char '\"'
                              , char '\\'
                              , char 'n' >> return '\n' 
                              , char 't' >> return '\t'
                              , char 'r' >> return '\r'
                              ]
                     ]
  char '"'
  return $ String x

parseHash :: Parser LispVal
parseHash = do
  char '#'
  choice [ char 't'  >> (return $ Bool True)
         , char 'f'  >> (return $ Bool False)
         , oneOf "odx" >>= parseNumber
         , parseChar
         ]

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ Atom atom

parseChar :: Parser LispVal
parseChar = do
  ch <- do try $ string "space"; return ' '
        <|> do try $ string "newline"; return '\n'
        <|> do { char '\''
               ; ch <- char '('
                       <|> char ' '
                       <|> letter
                       <|> digit
                       <|> symbol
               ; char '\''
               ; return ch
               }
  return $ Char ch

data Base = Binary | Octal | Decimal | Hexadecimal deriving (Eq)
--data Exact = Exact | Inexact | Unspecified deriving (Eq)

parseNumber :: Char -> Parser LispVal
--parseNumber = many1 digit >>= return . Number . read
--parseNumber = liftM (Number . read) $ many1 digit
parseNumber modify = do
  let base = case modify of 'o' -> Octal
                            'd' -> Decimal
                            'x' -> Hexadecimal
  if base == Decimal
    then (try $ parseInteger base) <|> parseFloat
    else (try $ parseInteger base)

parseInteger :: Base -> Parser LispVal
parseInteger base = do
  str <- many1 $ case base of
                      Octal -> octDigit
                      Decimal -> digit
                      Hexadecimal -> hexDigit
  notFollowedBy $ oneOf ".e"
  return . Number . fst . head . (case base of
                                   Octal       -> readOct
                                   Decimal     -> readDec
                                   Hexadecimal -> readHex) $ str

parseFloat :: Parser LispVal
parseFloat = do
  left  <- many1 digit
  mid   <- oneOf ".e" <|> return '.'
  right <- many1 digit <|> return "0"
  return . Float . fst . head . readFloat $ left ++ mid:right

parseList :: Parser LispVal
parseList = do
  char '('
  list <- endBy parseExpr spaces
  ls <- (char '.' >> spaces >> parseExpr >>= (return . DottedList list))
        <|> (return $ List list)
  char ')'
  return ls

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name)       = name
showVal (Number number)   = show number
showVal (Float number)    = show number
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (List ls)         = "(" ++ unwordsList ls ++ ")"
showVal (DottedList ls t) = "(" ++ unwordsList ls ++ " . " ++ showVal t ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

{-
instance Show LispVal where
  show = showVal
-}
