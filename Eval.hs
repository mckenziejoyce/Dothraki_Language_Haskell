module Eval where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Prelude
import Ast
import Lekh


-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed

run :: Ast -> (Choyo Val, [String])  -- ^ (error message or result value, all the printings)
run a = (runLekh (eval a) stdLib)

stdLib = Map.fromList
  [("tail", Fun $ \ v -> case v of Ls (_:ls) -> (Ok (Ls ls),[])
                                   _         -> ((Error "No tail having ass bitch"),[])),
   ("head", Fun $ \ v -> case v of Ls (h:_)      -> ((Ok h),[])
                                   _             -> ((Error "Sorry bad gag reflex"),[])),
   ("elem", Fun $ \ v -> case v of Fun f -> ((Error "You cant input a function"),[])
                                   _     -> (Ok (Fun $ \lst -> case lst of
                                                                  Ls ls -> (Ok (B (isInList v ls)),[])
                                                                  _     -> ((Error "You must input a list"),[])),[])),
   ("map", Fun $ \ v -> case v of Fun f -> (Ok (Fun $ \lst -> case lst of
                                                                Ls ls -> mapHelper f ls
                                                                _     -> ((Error "You must input a list"),[])),[])
                                  _ -> ((Error "You must input a function"),[])),
   ("filter", Fun $ \ v -> case v of BoolFun f -> (Ok (Fun $ \lst -> case lst of
                                                                   Ls ls -> (Ok (Ls (filterHelper f ls)),[])
                                                                   _     -> ((Error "You must input a list"),[])),[])
                                     _ -> ((Error "You must input a function"),[])),
   ("ord",  Fun $ \ v -> case v of C c -> (Ok (I (ordHelper c)), [])
                                   _   -> ((Error "Must insert a Char to ord"),[])),
   ("chr",  Fun $ \ v -> case v of I i -> (Ok (C (chrHelper i)), [])
                                   _   -> ((Error "Must insert a Integer to chr"),[])),
   ("float", Fun $ \ v -> case v of I f   -> ((Ok (F (toFloat f))),[])
                                    _     -> ((Error "Must input a Integer"),[])),
   ("int", Fun $ \ v -> case v of F f   -> ((Ok (I (toInt f))),[])
                                  _     -> ((Error "Must input a Float"),[]))
  ]
{-
"filter", SuperFun $ \ v -> \ l -> case v of
                                         BoolFun f -> case l of Ls ls -> (Ok (Ls (filterHelper f ls)),[])
                                                                _     -> ((Error "The second input must be a list"),[])
                                         _ -> ((Error "The first input must be an a -> Bool function"),[]))
-}





filterHelper :: (a -> Bool) -> [a] -> [a]
filterHelper f (x:[]) | (f x == True) = (x:[])
                      | otherwise = []
filterHelper f (x:xs) | (f x == True) = x:(filterHelper f xs)
                      | otherwise = (filterHelper f xs)
filterHelper f [] =[]

--NEED TO REWRITE THIS CONCAT ALL VALUES IN A LST AND CONCAT STRINGS --WRAP AND UNWRAP
mapHelper :: (Val -> (Choyo Val, [String])) -> [Val] -> (Choyo Val,[String])
mapHelper _ [] = (Ok (Ls []),[])
mapHelper f (x:[]) = case (f x) of ((Ok a), s) -> ((Ok (Ls [a])), s)
mapHelper f (x:xs) = case ((f x), (mapHelper f xs)) of (((Ok (Ls a)), s), ((Ok (Ls a2)), s2)) -> ((Ok (Ls (a ++ a2))), (s ++ s2))
{-
mapHelper f (x:[]) = ((f x): [])
mapHelper f (x:xs) = ([(f x)] ++ (mapHelper f xs))
mapHelper f [] = []
-}
isInList :: Eq a => a -> [a]-> Bool
isInList a (x:[]) | x == a = True
                  | otherwise = False
isInList a (x:xs) | x == a = True
                  | otherwise = (isInList a xs)
isInList a [] = False

toInt :: Float -> Integer
toInt x = truncate x

toFloat :: Integer -> Float
toFloat x = fromInteger x

type Env = Map String Val

eval :: Ast -> Lekh Env Val
eval (ValBool b) = return (B b)
eval (ValInt i) = return (I i)
eval (ValFloat f) = return (F f)
eval (ValChar c) = return (C c)
eval (ValString s) = return (S s)
eval (And x y) =
  do xres <- evalBool x
     yres <- evalBool y
     return (B (xres && yres))

eval (Or x y) =
  do xres <- evalBool x
     yres <- evalBool y
     return (B (xres || yres))

eval (Not x) =
  do xres <- evalBool x
     return (B (not xres))

eval (Plus x y) =
  do xres <- eval x
     yres <- eval y
     case xres of (F f) -> case yres of (F f2) -> return (F (f + f2))
                                        (I i)  -> err "Cant mix ints and floats"
                                        _ -> err "Can only perform Arithmetic on ints or floats"
                  (I i) -> case yres of (I f)  -> return (I (i + f))
                                        (F i2)  -> err "Cant mix ints and floats"
                                        _ -> err "Can only perform Arithmetic on ints or floats"
                  _ -> err "Can only perform Arithmetic on ints or floats"

eval (Minus x y) =
  do xres <- eval x
     yres <- eval y
     case xres of (F f) -> case yres of (F f2) -> return (F (f - f2))
                                        (I i)  -> err "Cant mix ints and floats"
                                        _ -> err "Can only perform Arithmetic on ints or floats"
                  (I i) -> case yres of (I f)  -> return (I (i - f))
                                        (F i2)  -> err "Cant mix ints and floats"
                                        _ -> err "Can only perform Arithmetic on ints or floats"
                  _ -> err "Can only perform Arithmetic on ints or floats"

eval (Mult x y) =
  do xres <- eval x
     yres <- eval y
     case xres of (F f) -> case yres of (F f2) -> return (F (f * f2))
                                        (I i)  -> err "Cant mix ints and floats"
                                        _ -> err "Can only perform Arithmetic on ints or floats"
                  (I i) -> case yres of (I f)  -> return (I (i * f))
                                        (F i2)  -> err "Cant mix ints and floats"
                                        _ -> err "Can only perform Arithmetic on ints or floats"
                  _ -> err "Can only perform Arithmetic on ints or floats"

eval (IntDiv x y) =
  do xres <- evalInt x
     yres <- evalInt y
     if yres == 0 then err "we are too divided" else return (I (xres `div` yres))

eval (FloatDiv x y) =
  do xres <- evalFloat x
     yres <- evalFloat y
     if yres == 0 then err "we are too divided" else return (F (xres / yres))

eval (IntExp x y) =
  do xres <- evalInt x
     yres <- evalInt y
     return (I (expp xres yres))

eval (FloatExp x y) =
  do xres <- evalFloat x
     yres <- evalFloat y
     return (F ((xres ** yres)))

eval (Mod x y) =
  do xres <- evalInt x
     yres <- evalInt y
     return (I (xres `mod` yres))

eval (Nil) =
  return (Ls [])

eval (Cons x y) =
  do x' <- eval x
     y' <- eval y
     case y' of Ls ls -> return $ Ls $ [x'] ++ ls
                _     -> err "ur such a con"
eval (Concat x y) =
  do x' <- eval x
     y' <- eval y
     case x' of Ls ls -> case y' of Ls ls2 -> return $ Ls $ ls ++ ls2
                                    _      -> err "second input not a list"
                _     -> err "first input not a list"
eval (If x y z) =
  do xres <- eval x
     case xres of
          B b -> if b then eval y else eval z
          I i -> if (i/=0) then eval y else eval z
          _ -> err "ERROR"
eval (Let v val bod) =
  do env <- getEnv
     val' <- eval val
     Lekh (\env -> runLekh (eval bod) (Map.insert v val' env))

eval (Lam x bod) =
  do env <- getEnv
     return $ Fun $ \  v ->  runLekh (eval bod) (Map.insert x v env)

eval (Var s) =
  do env <- getEnv
     case Map.lookup s (env) of
          Just var -> return var
          Nothing -> err "not the VARy cool"

eval (App x y) =
  do x' <- evalFun x
     y' <- eval y
     case x' y' of ((Ok a),s) -> return a
                   _ -> err "not hAPPy right now"

eval (Separator x y) =
  do x' <- eval x
     y' <- eval y
     return y'

eval (ListIndex l i) =
  do i' <- evalInt i
     l' <- evalList l
     let indx = genericIndex l' i'
     return indx

eval (Eq x y) =
  do x' <- eval x
     y' <- eval y
     case (x' == y') of True -> return (B True)
                        _    -> return (B False)
eval (Neq x y) =
  do x' <- eval x
     y' <- eval y
     case (x' == y') of False -> return (B True)
                        _     -> return (B False)
eval (Lt x y) =
  do x' <- eval x
     y' <- eval y
     case (x' > y') of False -> return (B True)
                       _     -> return (B False)
eval (Gt x y) =
  do x' <- eval x
     y' <- eval y
     case (x' > y') of True -> return (B True)
                       _     -> return (B False)
eval (Leq x y) =
  do x' <- eval x
     y' <- eval y
     case (x' > y') of False -> return (B True)
                       _     -> case (x' == y') of True -> return (B True)
                                                   _    -> return (B False)
eval (Geq x y) =
  do x' <- eval x
     y' <- eval y
     case (x' > y') of True -> return (B True)
                       _     -> case (x' == y') of True -> return (B True)
                                                   _    -> return (B False)
eval (UnaryMinus x) =
  do x' <- eval x
     case x' of (I i) -> return (I (-i))
                (F f) -> return (F (-f))
                _     -> err "Can only do Unary Minus on Ints and Floats"


eval (Print x) =
  do x' <- eval x
     Lekh (\e -> (Ok x', [show x']))
--   ^ This will have to be uncommenetd out once the monads are combined correctly

expp :: Integer -> Integer -> Integer
expp x 0 = 1
expp x y = x  * (expp x (y -1))

genericIndex :: (Integral i) => [a] -> i -> a
genericIndex (x:_)  0 = x
genericIndex (_:xs) n
 | n > 0     = genericIndex xs (n-1)
 | otherwise = errorWithoutStackTrace "List.genericIndex: negative argument."
genericIndex _ _      = errorWithoutStackTrace "List.genericIndex: index too large."


printyPrint:: Val -> Lekh Env Val
printyPrint x = undefined--Lekh $ \e -> case e of (Ok a, b) -> return (Ok a, b ++ x)


evalList :: Ast -> Lekh Env [Val]
evalList a =
  do res <- eval a
     case res of
          Ls ls -> return ls
          _ -> err "Not the ls we wanted </3"

evalInt :: Ast -> Lekh Env Integer
evalInt a =
  do res <- eval a
     case res of
          I i -> return i
          _ -> err "Not the int we wanted </3"

evalFloat :: Ast -> Lekh Env Float
evalFloat a =
  do res <- eval a
     case res of
          F f -> return f
          _ -> err "Not the float we wished for </3"

evalBool :: Ast -> Lekh Env Bool
evalBool a =
  do res <- eval a
     case res of
          B b -> return b
          _ -> err "Not the bool we deserved </3"

evalFun :: Ast -> Lekh Env (Val -> (Choyo Val, [String]))
evalFun a =
  do res <- eval a
     case res of
          Fun f -> return f
          --SuperFun f -> case f of (Val -> Val -> (Choyo Val, [String])) ->
          --NotFun f -> return f
          --BoolFun f -> return f
          _ -> err "This isn't fun anymore"
evalSuperFun :: Ast -> Lekh Env (Val -> Val -> (Choyo Val, [String]))
evalSuperFun a =
  do res <- eval a
     case res of
          SuperFun f -> return f
          _ -> err "This isn't superfun anymore"
{-
          | Fun (Val -> (Choyo Val,[String]))
          | SuperFun (Val -> Val -> (Choyo Val,[String]))
          | NotFun (Val -> Val)-- since this is a functional language, one thing that can be returned is a function
          | BoolFun (Val -> Bool)
-}
{-
boundVars :: Ast -> Set String
boundVars (Lam v bod) = Set.insert v $ boundVars bod
boundVars (App f a) = boundVars f `Set.union` boundVars a
boundVars x = Set.empty

findName :: String -> Set String -> String
findName str avoidNames | Set.member str avoidNames = findName (str ++ "'") avoidNames
                        | otherwise                 = str

rename :: Ast -> String -> String -> Ast
rename (App f a)      from to             = App (rename f from to) (rename a from to)
rename (Lam v bod)    from to | v == from = Lam v bod
rename (Lam v bod)    from to | otherwise = Lam v $ rename bod from to
--rename (FreeVar v)    from to | v == from = FreeVar to
--                              | otherwise = FreeVar v

-}
instance Eq Val where
  (I i) == (I x) = i == x
  (B b) == (B c) = b == c
  (F a) == (F b) = a == b
  (C a) == (C b) = a == b
  (S a) == (S b) = a == b
  (T a) == (T b) = a == b
  (Ls []) == (Ls []) = True
  (Ls (x:[])) == (Ls (y:[])) = x == y
  (Ls (x:xs)) == (Ls (y:ys)) = (x == y) && ((Ls xs) == (Ls ys))
  _ == _ = False

instance Ord Val where
  (I i) > (I x) = i > x
  --(I i) >= (I x) = i > x || i == x
  --(I i) < (I x) = i < x
  (B b) > (B c) = b > c
  --(B b) < (B c) = b < c
  (F a) > (F b) = a > b
  --(F a) < (F b) = a < b
  (C a) > (C b) = a > b
  --(C a) < (C b) = a < b
  (S a) > (S b) = a > b
  --(S a) < (S b) = a < b
  (T a) > (T b) = a > b
  --(T a) < (T b) = a < b
  (Ls a) > (Ls b) = a > b
  --(Ls a) < (Ls b) = a < b
  _ > _ = False



ordHelper :: Char -> Integer
ordHelper ' ' = 32
ordHelper '!' = 33
ordHelper '\"' = 34
ordHelper '#' = 35
ordHelper '$' = 36
ordHelper '%' = 37
ordHelper '&' = 38
ordHelper '\'' = 39
ordHelper '(' = 40
ordHelper ')' = 41
ordHelper '*' = 42
ordHelper '+' = 43
ordHelper ',' = 44
ordHelper '-' = 45
ordHelper '.' = 46
ordHelper '/' = 47
ordHelper '0' = 48
ordHelper '1' = 49
ordHelper '2' = 50
ordHelper '3' = 51
ordHelper '4' = 52
ordHelper '5' = 53
ordHelper '6' = 54
ordHelper '7' = 55
ordHelper '8' = 56
ordHelper '9' = 57
ordHelper ':' = 58
ordHelper ';' = 59
ordHelper '<' = 60
ordHelper '=' = 61
ordHelper '>' = 62
ordHelper '?' = 63
ordHelper '@' = 64
ordHelper 'A' = 65
ordHelper 'B' = 66
ordHelper 'C' = 67
ordHelper 'D' = 68
ordHelper 'E' = 69
ordHelper 'F' = 70
ordHelper 'G' = 71
ordHelper 'H' = 72
ordHelper 'I' = 73
ordHelper 'J' = 74
ordHelper 'K' = 75
ordHelper 'L' = 76
ordHelper 'M' = 77
ordHelper 'N' = 78
ordHelper 'O' = 79
ordHelper 'P' = 80
ordHelper 'Q' = 81
ordHelper 'R' = 82
ordHelper 'S' = 83
ordHelper 'T' = 84
ordHelper 'U' = 85
ordHelper 'V' = 86
ordHelper 'W' = 87
ordHelper 'X' = 88
ordHelper 'Y' = 89
ordHelper 'Z' = 90
ordHelper '[' = 91
ordHelper '\\' = 92
ordHelper ']' = 93
ordHelper '^' = 94
ordHelper '_' = 95
ordHelper '`' = 96
ordHelper 'a' = 97
ordHelper 'b' = 98
ordHelper 'c' = 91
ordHelper 'd' = 100
ordHelper 'e' = 101
ordHelper 'f' = 102
ordHelper 'g' = 103
ordHelper 'h' = 104
ordHelper 'i' = 105
ordHelper 'j' = 106
ordHelper 'k' = 107
ordHelper 'l' = 108
ordHelper 'm' = 109
ordHelper 'n' = 110
ordHelper 'o' = 111
ordHelper 'p' = 112
ordHelper 'q' = 113
ordHelper 'r' = 114
ordHelper 's' = 115
ordHelper 't' = 116
ordHelper 'u' = 117
ordHelper 'v' = 118
ordHelper 'w' = 119
ordHelper 'x' = 120
ordHelper 'y' = 121
ordHelper 'z' = 122
ordHelper '{' = 123
ordHelper '|' = 124
ordHelper '}' = 125
ordHelper '~' = 126
ordHelper _ = -1

chrHelper :: Integer -> Char
{-
chrHelper 0 = NUL
chrHelper 1 = SOH
chrHelper 2 = STX
chrHelper 3 = ETX
chrHelper 4 = EOT
chrHelper 5 = ENQ
chrHelper 6 = ACK
chrHelper 7 = BEL
chrHelper 8 = BS
chrHelper 9 = TAB
chrHelper 10 = LF
chrHelper 11 = VT
chrHelper 12 = FF
chrHelper 13 = CR
chrHelper 14 = SO
chrHelper 15 = SI
chrHelper 16 = DLE
chrHelper 17 = DC1
chrHelper 18 = DC2
chrHelper 19 = DC3
chrHelper 20 = DC4
chrHelper 21 = NAK
chrHelper 22 = SYN
chrHelper 23 = ETB
chrHelper 24 = CAN
chrHelper 25 = EM
chrHelper 26 = SUB
chrHelper 27 = ESC
chrHelper 28 = FS
chrHelper 29 = GS
chrHelper 30 = RS
chrHelper 31 = US
-}
chrHelper 32 = ' '
chrHelper 33 = '!'
chrHelper 34 = '\"'
chrHelper 35 = '#'
chrHelper 36 = '$'
chrHelper 37 = '%'
chrHelper 38 = '&'
chrHelper 39 = '\''
chrHelper 40 = '('
chrHelper 41 = ')'
chrHelper 42 = '*'
chrHelper 43 = '+'
chrHelper 44 = ','
chrHelper 45 = '-'
chrHelper 46 = '.'
chrHelper 47 = '/'
chrHelper 48 = '0'
chrHelper 49 = '1'
chrHelper 50 = '2'
chrHelper 51 = '3'
chrHelper 52 = '4'
chrHelper 53 = '5'
chrHelper 54 = '6'
chrHelper 55 = '7'
chrHelper 56 = '8'
chrHelper 57 = '9'
chrHelper 58 = ':'
chrHelper 59 = ';'
chrHelper 60 = '<'
chrHelper 61 = '='
chrHelper 62 = '>'
chrHelper 63 = '?'
chrHelper 64 = '@'
chrHelper 65 = 'A'
chrHelper 66 = 'B'
chrHelper 67 = 'C'
chrHelper 68 = 'D'
chrHelper 69 = 'E'
chrHelper 70 = 'F'
chrHelper 71 = 'G'
chrHelper 72 = 'H'
chrHelper 73 = 'I'
chrHelper 74 = 'J'
chrHelper 75 = 'K'
chrHelper 76 = 'L'
chrHelper 77 = 'M'
chrHelper 78 = 'N'
chrHelper 79 = 'O'
chrHelper 80 = 'P'
chrHelper 81 = 'Q'
chrHelper 82 = 'R'
chrHelper 83 = 'S'
chrHelper 84 = 'T'
chrHelper 85 = 'U'
chrHelper 86 = 'V'
chrHelper 87 = 'W'
chrHelper 88 = 'X'
chrHelper 89 = 'Y'
chrHelper 90 = 'Z'
chrHelper 91 = '['
chrHelper 92 = '\\'
chrHelper 93 = ']'
chrHelper 94 = '^'
chrHelper 95 = '_'
chrHelper 96 = '`'
chrHelper 97 = 'a'
chrHelper 98 = 'b'
chrHelper 99 = 'c'
chrHelper 100 = 'd'
chrHelper 101 = 'e'
chrHelper 102 = 'f'
chrHelper 103 = 'g'
chrHelper 104 = 'h'
chrHelper 105 = 'i'
chrHelper 106 = 'j'
chrHelper 107 = 'k'
chrHelper 108 = 'l'
chrHelper 109 = 'm'
chrHelper 110 = 'n'
chrHelper 111 = 'o'
chrHelper 112 = 'p'
chrHelper 113 = 'q'
chrHelper 114 = 'r'
chrHelper 115 = 's'
chrHelper 116 = 't'
chrHelper 117 = 'u'
chrHelper 118 = 'v'
chrHelper 119 = 'w'
chrHelper 120 = 'x'
chrHelper 121 = 'y'
chrHelper 122 = 'z'
chrHelper 123 = '{'
chrHelper 124 = '|'
chrHelper 125 = '}'
chrHelper 126 = '~'
--chrHelper 127 = DEL
chrHelper _ = ' '
