module Exec where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast
import Eval
import Parser
import ParserMonad
import Check
import Lekh



data LangOut =
    ParseError -- ^ retuned when the string could not be parsed
  | RuntimeError String [String]
  -- ^ retuned when there is a runtime error
  -- first String is the error message
  -- this list of Strings is what is printed before the error was encountered
  | Okk Val [String]
  -- ^ retuned when the program runs successfully and return a value
  -- The Val is the evaluation result of the program
  -- The list of String is what gets printed while running the program


-- | execute the program as a string and get the result
exec :: String -> LangOut
exec s = case (parse parser) s of
  Just (ast,"") -> case run ast of
                     (Ok v, []) -> (Okk v [])
                     (Ok v,s) -> (Okk v s)
                     (Error e, s) -> RuntimeError e s
                     _           -> ParseError
  _  -> ParseError


-- | perform static checking on the program string, may be empty if there is a parse error
warn :: String -> (Set WarningMsg)
warn s = case (exec s) of ParseError         -> Set.singleton FileParseError
                          RuntimeError e [s] -> let ast = case (parse parser) s of Just(a,_) -> a in (check ast)
                          Okk v [s]          -> Set.empty
                          _                  -> Set.empty
