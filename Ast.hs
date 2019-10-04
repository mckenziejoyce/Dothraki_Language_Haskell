module Ast where

import Lekh

-- | the abstract syntax tree for the language
data Ast = ValBool Bool | ValInt Integer | ValFloat Float
         | ValChar Char | ValString String

         | And Ast Ast | Or Ast Ast | Not Ast

         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | IntDiv Ast Ast | FloatDiv Ast Ast
         | Mod Ast Ast | FloatExp Ast Ast | IntExp Ast Ast

         | Nil
         | Cons Ast Ast
         | ListIndex Ast Ast
         | Concat Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | App Ast Ast
         | Print Ast
         | Separator Ast Ast
         | UnaryMinus Ast

         | Eq Ast Ast
         | Neq Ast Ast
         | Lt Ast Ast
         | Gt Ast Ast
         | Leq Ast Ast
         | Geq Ast Ast
           deriving (Eq,Show) -- helpful to use this during testing
--         deriving Eq

--instance Show Ast where
  --show ast = showPretty ast 0


-- the goal of the program is to return a value
data Val = I Integer | B Bool | F Float
         | C Char | S String
         | T (Val, Val)
         | Ls [Val]
         | Fun (Val -> (Choyo Val,[String]))
         | SuperFun (Val -> Val -> (Choyo Val,[String]))
         | NotFun (Val -> Val)-- since this is a functional language, one thing that can be returned is a function
         | BoolFun (Val -> Bool)


instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (F f) = show f
  show (C c) = show c
  show (S s) = show s
  show (T t) = show t
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function
  show (SuperFun _) = "\\ x -> \\ y -> ?"
  show (NotFun _) = "..uh"

-- | output the fully parenthesized statement
showFullyParen :: Ast -> String
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (ValFloat f) = "(" ++ show f ++ ")"
showFullyParen (ValString s) = "(" ++ show s ++ ")"
showFullyParen (ValChar c) = "(" ++ show c ++ ")"
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (Print a) = "(print " ++ showFullyParen a ++ ")"
showFullyParen (UnaryMinus a) = "(- " ++ (showFullyParen a) ++ ")"
showFullyParen (Separator l r) = "(" ++ (showFullyParen l) ++ " ; " ++ (showFullyParen r) ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (Geq l r) = "(" ++ (showFullyParen l) ++ " >= " ++ (showFullyParen r) ++ ")"
showFullyParen (Leq l r) = "(" ++ (showFullyParen l) ++ " <= " ++ (showFullyParen r) ++ ")"
showFullyParen (Gt l r) = "(" ++ (showFullyParen l) ++ " > " ++ (showFullyParen r) ++ ")"
showFullyParen (Lt l r) = "(" ++ (showFullyParen l) ++ " < " ++ (showFullyParen r) ++ ")"
showFullyParen (Neq l r) = "(" ++ (showFullyParen l) ++ " /= " ++ (showFullyParen r) ++ ")"
showFullyParen (Eq l r) = "(" ++ (showFullyParen l) ++ " == " ++ (showFullyParen r) ++ ")"
showFullyParen (Not a) = "(" ++ " ! " ++ (showFullyParen a) ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (FloatDiv l r) = "(" ++ (showFullyParen l) ++ " / " ++ (showFullyParen r) ++ ")"
showFullyParen (IntDiv l r) = "(" ++ (showFullyParen l) ++ " // " ++ (showFullyParen r) ++ ")"
showFullyParen (Mod l r) = "(" ++ (showFullyParen l) ++ " % " ++ (showFullyParen r) ++ ")"
showFullyParen (FloatExp l r) = "(" ++ (showFullyParen l) ++ " ^ " ++ (showFullyParen r) ++ ")"
showFullyParen (IntExp l r) = "(" ++ (showFullyParen l) ++ " ** " ++ (showFullyParen r) ++ ")"
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s) = "( " ++ s ++ ")"
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen (Concat xs ys) = "(" ++ (showFullyParen xs) ++ " ++ " ++ (showFullyParen ys) ++ ")"
showFullyParen (ListIndex ls n) = "(" ++ (showFullyParen ls) ++ " !! " ++ (showFullyParen n) ++ ")"
showFullyParen Nil = "([])"


showPretty :: Ast -> Integer -> String
showPretty (ValInt i) _ =  if i < 0
                           then  "(" ++ show i ++ ")"
                           else show i
showPretty (ValBool True) _ =  "true"
showPretty (ValBool False)  _  = "false"
showPretty (ValFloat f) _ = if f < 0
                            then "(" ++ show f ++ ")"
                            else show f
showPretty (ValString s) _ = show s
showPretty (ValChar c) _ = show c
showPretty Nil _ = "[]"
showPretty (Var s) _ = s
showPretty (Lam v bod) i = parenthesize 1 i $ "\\ " ++ v ++ " -> " ++ (showPretty bod 100)
showPretty (Let v a bod)  i = parenthesize 1 i $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i = parenthesize 1 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)
showPretty (Separator l r) i = parenthesize 2 i $  (showPretty l 2) ++ " ; " ++ (showPretty r 3)
showPretty (App l r) i = parenthesize 4 i $ (showPretty l 4) ++ " " ++ (showPretty r 5)
showPretty (Or l r) i = parenthesize 6 i $ (showPretty l 6) ++ " || " ++ (showPretty r 7)
showPretty (And l r) i = parenthesize 8 i $ (showPretty l 8) ++ " && " ++ (showPretty r 9)
showPretty (Gt l r) i = parenthesize 10 i $ (showPretty l 11) ++ " > " ++ (showPretty r 11)
showPretty (Lt l r) i = parenthesize 10 i $ (showPretty l 11) ++ " < " ++ (showPretty r 11)
showPretty (Geq l r) i = parenthesize 10 i $ (showPretty l 11) ++ " >= " ++ (showPretty r 11)
showPretty (Leq l r) i = parenthesize 10 i $ (showPretty l 11) ++ " <= " ++ (showPretty r 11)
showPretty (Neq l r) i = parenthesize 10 i $ (showPretty l 11) ++ " /= " ++ (showPretty r 11)
showPretty (Eq l r) i = parenthesize 10 i $ (showPretty l 11) ++ " == " ++ (showPretty r 11)
showPretty (Cons l r) i = parenthesize 12 i $ (showPretty l 13) ++ " : " ++ (showPretty r 12)
showPretty (Concat xs ys) i = parenthesize 12 i $ (showPretty xs 13) ++ " ++ " ++ (showPretty ys 12)
showPretty (Minus l r) i = parenthesize 14 i $ (showPretty l 14) ++ " - " ++ (showPretty r 15)
showPretty (Plus l r) i = parenthesize 14 i $ (showPretty l 14) ++ " + " ++ (showPretty r 15)
showPretty (Mult l r) i = parenthesize 16 i $ (showPretty l 16) ++ " * " ++ (showPretty r 17)
showPretty (FloatDiv l r) i = parenthesize 16 i $ (showPretty l 16) ++ " / " ++ (showPretty r 17)
showPretty (IntDiv l r) i = parenthesize 16 i $ (showPretty l 16) ++ " // " ++ (showPretty r 17)
showPretty (Mod l r) i = parenthesize 16 i $ (showPretty l 16) ++ " % " ++ (showPretty r 17)
showPretty (FloatExp l r) i = parenthesize 18 i $ (showPretty l 19) ++ " ^ " ++ (showPretty r 18)
showPretty (IntExp l r) i = parenthesize 18 i $   (showPretty l 19) ++ " ** " ++ (showPretty r 18)
showPretty (ListIndex ls n) i = parenthesize 20 i $ (showPretty ls 20) ++ " !! " ++ (showPretty n 21)
showPretty (Not l) i = parenthesize 30 i $  " ! " ++ (showPretty l 30)
showPretty (UnaryMinus l) i = parenthesize 30 i $  "(- " ++ (showPretty l 30) ++ ")"
showPretty (Print s) i = parenthesize 30 i $ "(print " ++ (showPretty s 30) ++ ")"


parenthesize :: Integer -- ^ the precedence level of outer expression
              -> Integer -- ^ the precedence level of the current expression
              -> String -- ^ string representation current expression
              -> String -- ^ the properly (not necessarily fully) parenthesized current expression

parenthesize outerLevel curLevel showExp
  | outerLevel < curLevel = "(" ++ showExp ++ ")"
  | otherwise             =        showExp
