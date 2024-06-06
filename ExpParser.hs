module ExpParser (Expr(..), exprParser) where

import Text.Parsec
import Text.Parsec.String (Parser)

-- Estrutura de dados para representar expressões lógicas
data Expr
    = Atom Char
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | Imply Expr Expr
    | BiImply Expr Expr
    deriving (Show, Eq)

-- Função para lidar com expressões atômicas
atomParser :: Parser Expr
atomParser = Atom <$> letter

-- Função para lidar com operadores de negação
notParser :: Parser Expr
notParser = do
    _ <- char '~'
    Not <$> factorParser

-- Função para lidar com operador de conjunção
andParser :: Parser (Expr -> Expr -> Expr)
andParser = do
    _ <- char '&'
    return And

-- Função para lidar com operador de disjunção
orParser :: Parser (Expr -> Expr -> Expr)
orParser = do
    _ <- char '|'
    return Or

-- Função para lidar com operador de implicação
implyParser :: Parser (Expr -> Expr -> Expr)
implyParser = do
    _ <- char '>'
    return Imply

biImplyParser :: Parser (Expr -> Expr -> Expr)
biImplyParser = do
    _ <- char '$'
    return BiImply

-- Função para lidar com fatores
factorParser :: Parser Expr
factorParser = atomParser
           <|> notParser
           <|> between (char '(') (char ')') exprParser

-- Função para lidar com termos
termParser :: Parser Expr
termParser = chainl1 factorParser (andParser <|> orParser <|> implyParser <|> biImplyParser)

-- Expressão principal
exprParser :: Parser Expr
exprParser = termParser
