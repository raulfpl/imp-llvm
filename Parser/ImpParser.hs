module Parser.ImpParser(parser) where

import Control.Applicative hiding ((<|>), Const)
import Data.Functor
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import Syntax.ImpSyntax

-- main parse function

parser :: String -> Either ParseError Program
parser = parse impParser ""

-- lexer definition

impStyle :: LanguageDef st
impStyle = emptyDef {
                commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "|&=:<"
              , opLetter = oneOf "|&=:<"
              , reservedOpNames = ["|", "&", "=", ":=", "+", "-", "*", "/", "<="]
              , reservedNames = ["true", "false", "skip", "not", "int", "bool",
                                 "if", "then", "else", "var" , "program",
                                 "while"]
           }   

-- lexer definition

lexer :: TokenParser st
lexer = makeTokenParser impStyle

-- parser definition

bexpParser :: Parser BExp
bexpParser = buildExpressionParser table term <?> "expression"
             where
               table = [ 
                         [Prefix (reserved lexer "not" >> (return Not))]
                       ,
                         [Infix (reservedOp lexer "&" >> (return (:&:))) AssocLeft
                         , Infix (reservedOp lexer "|" >> (return (:|:))) AssocLeft]
                       , [Infix (reservedOp lexer "=" >> (return (:=:))) AssocNone
                         , Infix (reservedOp lexer "<=" >> (return (:<=:))) AssocNone]
                       ]

term :: Parser BExp
term = const (BConst True)  <$> reserved lexer "true"   <|>
       const (BConst False) <$> reserved lexer "false"  <|>
       BStr <$> stringLiteral lexer                     <|>
       (parens lexer bexpParser)                        <|>
       BNum <$> fact

fact :: Parser AExp
fact = buildExpressionParser table nums <?> "numeric expression"
       where
          table = [ [Infix (reserved lexer "+" >> (return (:+:))) AssocLeft, 
                    Infix (reserved lexer "-" >> (return (:-:))) AssocLeft]
                  , [Infix (reserved lexer "*" >> (return (:*:))) AssocLeft, 
                     Infix (reserved lexer "/" >> (return (:/:))) AssocLeft]
                  ]
          nums = Var <$> identifier lexer                <|> 
                 (Const . fromInteger) <$> integer lexer <|> 
                 (parens lexer fact)

comParser :: Parser Com
comParser = const Skip <$> reserved lexer  "skip" <|>
            Print <$> bexpParser          <|>
            Read  <$> identifier lexer    <|>
            Assign <$> identifier lexer <*> bexpParser <|>
            ifParser <|> whileParser

block :: Parser [Com]
block = braces lexer (endBy comParser (semi lexer))

ifParser :: Parser Com
ifParser = (\ e bs _ bs' -> If e bs bs') <$> bexpParser <*> 
                                             block <*> 
                                             reserved lexer "else" <*> block   

whileParser :: Parser Com    
whileParser = While <$> bexpParser <*> block       

varDefParser :: Parser VarDef
varDefParser = VarDef <$> tyParser <*> identifier lexer

tyParser :: Parser Ty
tyParser = const TyInt <$> reserved lexer "int" <|>
           const TyBool <$> reserved lexer "bool"

varDefsParser :: Parser [VarDef]
varDefsParser = option [] (reserved lexer "var" *> 
                           braces lexer (endBy1 varDefParser (semi lexer))) 

impParser :: Parser Program
impParser = (\ v _ p -> Program v p) <$> varDefsParser <*> reserved lexer "program" <*> block
