module PrologParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char (isLower, isUpper)
import PrologAst

languageDef =
  emptyDef { Token.identStart = lower,
             Token.identLetter = alphaNum <|> char '_',
             Token.reservedNames = ["module", "type"],
             Token.reservedOpNames = [",", ";", "->", ":-"]
           }

lexer = Token.makeTokenParser languageDef

identifier = do
  i <- Token.identifier lexer
  guard $ isLower $ head i
  return i

var :: Parser [Char]
var = do
  h <- upper
  t <- many (alphaNum <|> char '_')
  spaces
  return (h:t)

whiteSpace = Token.whiteSpace lexer
reservedOp = Token.reservedOp lexer
reserved = Token.reserved lexer
brackets = Token.parens lexer
dot = Token.dot lexer

parseAtom :: Parser Atom
parseAtom =
   try (do
    spaces
    i <- identifier
    spaces
    t <- parseTail
    spaces
    return $ IDTAIL i t
   ) <|>
   try (do
   spaces
   i <- identifier
   spaces
   return $ ID i
   )

parseTail :: Parser Tail
parseTail = 
  try (do
   spaces
   ab <- parseAtomBr
   spaces
   t <- parseTail
   spaces
   return $ ATOMBRTAIL ab t
  ) <|>
  try (do
   spaces
   ab <- parseAtomBr
   spaces
   return $ ATOMWITHBR ab
  ) <|>
  try (do
   spaces
   v <- var
   spaces
   t <- parseTail
   spaces
   return $ VARTAIL v t
  ) <|>
  try (do
    spaces
    v <- var
    spaces
    return $ VAR v
  ) <|>
  try (do
   spaces
   a <- parseAtom
   spaces
   return $ ATOM a
  )

parseAtomBr :: Parser AtomBr
parseAtomBr = 
  try (do
    spaces
    char '('
    spaces
    a <- parseAtom
    spaces
    char ')'
    spaces
    return $ ATOMINBR a
  ) <|>
  try (do
    spaces
    char '('
    spaces
    abr <- parseAtomBr
    spaces
    char ')'
    spaces
    return $ ATOMBR abr
  ) <|>
  try (do
    spaces
    char '('
    spaces
    v <- var
    spaces
    char ')'
    spaces
    return $ VARBR v
  )

-- for mode '--relation':
parseRelationFull :: Parser Relation
parseRelationFull =
  do
    spaces
    a <- parseAtom
    spaces
    reservedOp ":-"
    spaces
    b <- parseRelationBody
    spaces
    dot
    spaces
    return $ BODY a b

parseRelation :: Parser Relation
parseRelation =
  try parseRelationFull <|>
  try (do
    spaces
    a <- parseAtom
    spaces
    dot
    spaces
    return $ HEAD a
  )

parseRelationBody :: Parser RelationBody
parseRelationBody = fmap (foldl1 Disj) (do 
  h <- parseConj
  t <- many (reservedOp ";" >> parseConj)
  return (h:t)
  )

parseConj = fmap (foldl1 Conj) (do
  h <- parseOperand
  t <- many (reservedOp "," >> parseOperand)
  return (h:t)
  )

parseOperand =
  try (do
    a <- parseAtom
    return $ RAtom a
  ) <|>
  try (do
    spaces
    char '('
    spaces
    rb <- parseRelationBody
    spaces
    char ')'
    spaces
    return $ RBodyBr rb
  )


parseModule =
  try (do
    spaces
    reserved "module"
    spaces
    name <- identifier
    spaces
    dot
    return name
  )

parseType :: Parser Type
parseType =
  try (do
    spaces
    v <- var
    spaces
    return $ Var v
  ) <|>
  try (do
    spaces
    a <- parseAtom
    spaces
    return $ TAtom a
  ) <|>
  try (do
    spaces
    char '('
    spaces
    t <- parseArrow
    spaces
    char ')'
    spaces
    return $ TBr t
  )

parseArrow = fmap (foldl1 Arrow) (do
    h <- parseType
    t <- many (reservedOp "->" >> parseType)
    return (h:t)
  )

parseTypeDef :: Parser TypeDef
parseTypeDef = do
  spaces
  reserved "type"
  spaces
  name <- identifier
  spaces
  t <- parseArrow
  spaces
  dot
  spaces
  return $ TypeDef name t

parseProg :: Parser PrologProgram
parseProg = do
  pMod <- optionMaybe parseModule
  types <- many parseTypeDef
  rs <- many parseRelation
  return $ Program pMod types rs

parseString :: String -> Either ParseError PrologProgram
parseString =
  parse (do r <- parseProg; eof; return r) ""

parseFromFileByMode path parseMode = do
  input <- readFile path
  case (parse (do r <- parseMode; eof; return r) "" input) of
    Left err -> print err
    Right r -> do
      writeFile (path ++ ".out") (show r)
      