{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Functor.Identity (Identity)
import qualified Data.Text             as T
import           LispVal
import           Text.Parsec
import qualified Text.Parsec.Language  as Lang
import           Text.Parsec.Text
import qualified Text.Parsec.Token     as Tok

-- newtype Parser LispVal = Parser (Text -> [(LispVal, Text)])

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style =
  Lang.emptyDef
    { Tok.commentStart = "{-"
    , Tok.commentEnd = "-}"
    , Tok.commentLine = "--"
    , Tok.opStart = Tok.opLetter style
    , Tok.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
    , Tok.identStart = letter <|> oneOf "-+/*=|&><"
    , Tok.identLetter = digit <|> letter <|> oneOf "?+=|&-/"
    , Tok.reservedOpNames = ["'", "\""]
    }

m_parens :: ParsecT T.Text () Identity a -> ParsecT T.Text () Identity a
m_identifier :: ParsecT T.Text () Identity String
Tok.TokenParser
  { Tok.parens = m_parens,
    Tok.identifier = m_identifier
  } = lexer

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer $ T.unpack op

parseAtom :: Parser LispVal
parseAtom = do
  p <- m_identifier
  return $ Atom $ T.pack p

parseText :: Parser LispVal
parseText = do
  reservedOp "\""
  p <- many1 $ noneOf "\""
  reservedOp "\""
  return $ String . T.pack $ p

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseNegNum :: Parser LispVal
parseNegNum = do
  char '-'
  d <- many1 digit
  return $ Number . negate . read $ d

parseList :: Parser LispVal
parseList =
  List . concat <$> Text.Parsec.many parseExpr
    `sepBy` (char ' ' <|> char '\n')

parseSExp :: Parser LispVal
parseSExp =
  List . concat
    <$> m_parens
      ( Text.Parsec.many parseExpr
          `sepBy` (char ' ' <|> char '\n')
      )

parseQuote :: Parser LispVal
parseQuote = do
  reservedOp "\'"
  x <- parseExpr
  return $ List [Atom "quote", x]

parseReserved :: Parser LispVal
parseReserved =
  do
    reservedOp "Nil" >> return Nil
    <|> (reservedOp "#t" >> return (Bool True))
    <|> (reservedOp "#f" >> return (Bool False))

parseExpr :: Parser LispVal
parseExpr =
      parseReserved
  <|> parseNumber
  <|> try parseNegNum
  <|> parseAtom
  <|> parseText
  <|> parseQuote
  <|> parseSExp

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseList) "<file>"
