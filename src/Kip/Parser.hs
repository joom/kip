{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Kip.Parser where

import Kip.AST

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Expr as P

data ParserCtx =
  ParserCtx
    { ctx :: [(Name, Case)]
    }

emptyParserCtx :: ParserCtx
emptyParserCtx = ParserCtx []

-- We need [IO] access in here because we need morphological parsing.
type Outer = IO
type KipParser = ParsecT String ParserCtx Outer

period :: KipParser ()
period = spaces >> string "." >> spaces

lexeme :: KipParser a -> KipParser a
lexeme p = spaces *> p <* spaces

parens :: KipParser a -> KipParser a
parens p = char '(' *> p <* char ')'

name :: KipParser Name
name = (:) <$> letter <*> many (digit <|> letter)

word :: KipParser Name
word = many1 letter

multiword :: KipParser Name
multiword = unwords <$> many1 (lexeme word)


-- Start copied from https://stackoverflow.com/a/24106749/2016295, CC BY-SA 3.0
escape :: KipParser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: KipParser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: KipParser String
character = fmap return nonEscape <|> escape

parseQuotedString :: KipParser String
parseQuotedString = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings
-- End copied from https://stackoverflow.com/a/24106749/2016295, CC BY-SA 3.0

identifier :: KipParser Name
identifier = word <|> parens multiword

parseExp :: KipParser Exp
parseExp = try var <|> parens parseExp <|> str 
  where
    var = Var <$> identifier 
    str = StrLit <$> parseQuotedString

parseStmt :: KipParser Stmt
parseStmt =
  ty <|> expFirst
  where
    ctor = try ((, []) <$> identifier) 
      -- <|> parens (do 
      -- n <- identifier 
      -- -- TODO many parseTy
      -- return (n, [("arg", (TyString, Nom))]))
    ya = lexeme (string "ya")
    ty = do
      lexeme (string "Bir")
      n <- identifier
      ctors <- try (lexeme (string "var olamaz") *> return [])
           <|> ((ya *> sepBy1 ctor (try ya)) <* lexeme (string "olabilir"))
      period
      return (NewType n ctors)
    expFirst = do
      e <- parseExp
      (try (lexeme (string "yazdır") *> return (Print e)) <|> 
        return (ExpStmt e)) <* period

parseFromRepl :: String -> Outer (Either ParseError Stmt)
parseFromRepl input = runParserT parseStmt emptyParserCtx "Kip" input
