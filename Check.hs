module Check where

import Data.Set
import qualified Data.Set as Set

import Ast
import Lekh
import Eval

-- here you can preform static checks


-- | The data type for all the static check warning
-- Some example include:
--   * use of undefined variable
--   * defined but unused variable
--   * type errors
data WarningMsg =
    UndefinedVarUse String  -- ^ This is the Warning for use of Undefined variable name  --Warns when a variable is used but not decalared
    | UnusedVar String
    | TypeErr String
    | FileParseError
  -- ...
  deriving (Show,Eq,Ord)

-- | perform static checking on the Ast ----- to add more u could have a helper function that checks for each error type and union that shit
-- the output a set of warning on that input Ast
check :: Ast -> Set WarningMsg
check x =
  do case (isClosed x) of False -> case (size (freeVars x)) of 1 -> Set.singleton (UndefinedVarUse (elemAt 0 (freeVars x)))
                                                               _ -> fromList([(UndefinedVarUse k)|w <- [0..((size (freeVars x))-1)], k <- [elemAt w (freeVars x)]])
                          True  -> Set.empty



-- collect all the vars that appear in the expression that are not bound
freeVars :: Ast -> Set String
freeVars (Var a) = Set.singleton a
freeVars (ValBool a) = Set.empty
freeVars (ValInt a) = Set.empty
freeVars (ValFloat a) = Set.empty
freeVars (ValChar a) = Set.singleton [a]
freeVars (ValString a) = Set.singleton a
freeVars (App a b) = freeVars a `Set.union` freeVars b
freeVars (Lam s t) = Set.delete s $ freeVars t
freeVars (Mult a b) = freeVars a `Set.union` freeVars b
freeVars (Plus a b) = freeVars a `Set.union` freeVars b
freeVars (Minus a b) = freeVars a `Set.union` freeVars b
freeVars (IntDiv a b) = freeVars a `Set.union` freeVars b
freeVars (FloatDiv a b) = freeVars a `Set.union` freeVars b
freeVars (Mod a b) = freeVars a `Set.union` freeVars b
freeVars (FloatExp a b) = freeVars a `Set.union` freeVars b
freeVars (IntExp a b) = freeVars a `Set.union` freeVars b
freeVars Nil = Set.empty
freeVars (Cons a b) = freeVars a `Set.union` freeVars b
freeVars (ListIndex a b) = freeVars a `Set.union` freeVars b
freeVars (Concat a b) = freeVars a `Set.union` freeVars b
freeVars (And a b) = freeVars a `Set.union` freeVars b
freeVars (Or a b) = freeVars a `Set.union` freeVars b
freeVars (Not a) = freeVars a
freeVars (Eq a b) = freeVars a `Set.union` freeVars b
freeVars (Neq a b) = freeVars a `Set.union` freeVars b
freeVars (Lt a b) = freeVars a `Set.union` freeVars b
freeVars (Gt a b) = freeVars a `Set.union` freeVars b
freeVars (Leq a b) = freeVars a `Set.union` freeVars b
freeVars (Geq a b) = freeVars a `Set.union` freeVars b
freeVars (Separator a b) = freeVars a `Set.union` freeVars b
freeVars (Let s a t) = (Set.delete s (freeVars t)) `Set.union` freeVars a
freeVars (If a b c) = freeVars a `Set.union` freeVars b `Set.union` freeVars c
freeVars (UnaryMinus a) = freeVars a
freeVars (Print a) = freeVars a


--No Free Vars in the exp
isClosed :: Ast -> Bool
isClosed (Var _) = False
isClosed x = Set.null (freeVars x)
{-
check :: Ast -> Set WarningMsg
check x =
  do case (checkHelper x) of (Error "No error") -> NoError
                             (Error "Cant mix ints and floats") -> TypeErr "The inputs to this function must be the same type"



checkHelper :: Ast -> Choyo Val
checkHelper x =
  do case (run x) of (Error e, _) -> Error e
                     _            -> Error "No error"

-}
