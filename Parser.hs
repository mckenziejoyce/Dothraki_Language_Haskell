module Parser where

import Ast
import ParserMonad
import Data.Char


parser :: Parser Ast
parser = seps

seps :: Parser Ast
seps = withInfix apps [(";", Separator)]

apps :: Parser Ast
apps = withInfix orExpr [("",App)]

orExpr :: Parser Ast
orExpr = withInfix andExpr [("||", Or)]

andExpr :: Parser Ast
andExpr = withInfix boolArith [("&&", And)]

boolArith :: Parser Ast
boolArith = withInfix consConcat [("==", Eq), ("/=", Neq), ("<=", Leq), (">=", Geq), (">", Gt), ("<", Lt)]

consConcat :: Parser Ast
consConcat = do x <- addSubExpr
                (consConcatHelper x <|> return x)

consConcatHelper :: Ast -> Parser Ast
consConcatHelper left = do s <- token $ (literal ":") <||> (literal "++")
                           right <- consConcat
                           let res = case s of Left _ -> (Cons left right)
                                               Right _ -> (Concat left right)
                           return res

addSubExpr :: Parser Ast
addSubExpr = withInfix multDivExpr [("+", Plus), ("-", Minus)]

multDivExpr :: Parser Ast
multDivExpr = withInfix expExpr [("*", Mult), ("//", IntDiv), ("/", FloatDiv), ("%", Mod)]

expExpr :: Parser Ast
expExpr = do x <- listIndexExpr
             (expHelper x <|> return x)

expHelper :: Ast -> Parser Ast
expHelper left = do s <- token $ (literal "^") <||> (literal "**")
                    right <- expExpr
                    let res = case s of Left _ ->  (FloatExp left right)
                                        Right _ -> (IntExp left right)
                    return res

listIndexExpr :: Parser Ast
listIndexExpr = withInfix prefix [("!!", ListIndex)]


notExp :: Parser Ast
notExp = (do token $ literal "!"
             a <- prefix
             return $ Not a)

negExp :: Parser Ast
negExp = (do token $ literal "("
             token $ literal "-"
             a <- prefix
             token $ literal ")"
             return $ UnaryMinus a)

prints :: Parser Ast
prints = do token $ literal "(print"
            ast <- parser
            token $ literal ")"
            return (Print ast)

prefix = notExp <|> negExp <|> prints <|> atoms

atoms:: Parser Ast
atoms = emptyString <|> strings <|> chars <|> floats <|> ints <|> bools <|> nil <|>  vars <|> lambdaParser <|> letParser <|> ifParser  <|> parens


emptyString :: Parser Ast
emptyString = do token $ literal "\""
                 token $ literal "\""
                 return (ValString "")

strings :: Parser Ast
strings = do token $ literal "\""
             i <-  rep $ sat (\c -> c /= '"')
             token $ literal "\""
             return (ValString (i))

chars :: Parser Ast
chars = do token $ literal "'"
           i <-  alpha
           token $ literal "'"
           return (ValChar (i))

floats :: Parser Ast
floats = do p <- token $ some digit
            token $ literal "."
            d <- token $ some digit
            return $ ValFloat ((read (p ++ "." ++ d)):: Float)

ints :: Parser Ast
ints = do s <- token $ intParser
          return $ ValInt s

boolean = ["true", "false"]

bools :: Parser Ast
bools = do x <- token $ varParser
           if (x `elem` boolean)
            then (if x == "true" then return (ValBool True) else return (ValBool False))
            else failParse

nil :: Parser Ast
nil = do token $ literal "["
         token $ literal "]"
         return Nil

keywords = ["if","then","else", "let", "in", "true","false", "print", "not"]

vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

lambdaParser :: Parser Ast
lambdaParser = do token $ literal "\\"
                  x <- varParser
                  token $ literal "->"
                  y <- parser
                  return (Lam x y)

letParser :: Parser Ast
letParser = do token $ literal "let"
               x <- varParser
               token $ literal "="
               y <- parser
               token $ literal "in"
               z <- parser
               return (Let x y z)

ifParser :: Parser Ast
ifParser = do token $ literal "if"
              x <- parser
              token $ literal "then"
              y <- parser
              token $ literal "else"
              z <- parser
              return (If x y z)

parens :: Parser Ast
parens = do token $ literal "("
            x <- parser
            token $ literal ")"
            return x
