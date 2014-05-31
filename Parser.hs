module Parser where

import Control.Monad (liftM)
import Control.Applicative hiding (many, (<|>))
import Text.Parsec
import Text.Parsec.String (Parser)
import Lexer
import AST

acomma = lexeme comma

aparens = lexeme (symbol ")")

baldString = lexeme . fmap String $
   (:) <$> noneOf "? ,)"   
       <*> many (noneOf " ,)")   -- problematic - see comment below
       <* acomma

number = lexeme . fmap Number $
    read <$> many1 digit
         <* acomma

niceTuple = Tuple <$> lexeme resvd_cmd <* lexeme (char '(')
                  <*> lexeme ident <* acomma
                  <*> many ( number <|> baldString )
                  <*> many queries

ident = liftM Identifier identifier <?> "WorkerName"

queries = lexeme . fmap Query $
         (:) <$> oneOf "?"
             -- <*> many letter
             <*> many (noneOf " ,)") 
             <*  (  aparens <|> acomma )

resvd_cmd = do { reserved "rd"; return ("rd") }
            <|> do { reserved "eval"; return ("eval") }
            <|> do { reserved "read"; return ("read") }
            <|> do { reserved "in"; return ("in") }
            <|> do { reserved "out"; return ("out") }
            <?> "LINDA-like Tuple"
