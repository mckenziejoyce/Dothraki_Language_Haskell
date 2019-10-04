module Lekh where

import Control.Monad(ap)

data Lekh env a = Lekh (env  ->(Choyo a, [String]))

data Choyo a = Error String | Ok a deriving (Show, Eq)

-- function that just runs the contents of Lekh
runLekh :: (Lekh e a) -> e -> (Choyo a, [String])
runLekh (Lekh f) env = f env


-- a way to easily return an error (for instance in do notation)
err :: String -> Lekh e a
err s = Lekh $ \ _ -> (Error s, [])

-- a way to easily get the entire environment (for instance in do notation)
getEnv :: Lekh e e
getEnv = Lekh $ \ env -> (Ok env, [])


instance Functor (Lekh e) where
  fmap f (Lekh env) = Lekh $ \e -> case (env e) of (Ok a, b) -> (Ok (f a), b)
                                                   (Error s, b) -> (Error s, b)

--ignore this for now
instance Applicative (Lekh e) where
  pure = return
  (<*>) = ap


instance Monad (Lekh e) where
  return a = Lekh $ \e -> (Ok a, [])

  (Lekh env) >>= f = Lekh $ \e -> case (env e) of (Ok a, b) -> let Lekh g = f a in (concatP (g e) (env e))
                                                  (Error s, b) -> (Error s, b)



concatP :: (Choyo a, [String]) -> (Choyo b, [String])-> (Choyo a, [String])
concatP (Error a, []) _ = (Error a, [])
concatP _ (Error b, []) = (Error b, [])
concatP (Error a, s) _ = (Error a, s)
concatP _ (Error b, s) = (Error b, s)
concatP (Ok a, []) (Ok c, []) = (Ok a, [])
concatP (Ok a, []) (Ok c, [d]) = (Ok a, [d])
concatP (Ok a, [b]) (Ok c, []) = (Ok a, [b])
concatP (Ok a, [""]) (Ok c, [d]) = (Ok a, [d])
concatP (Ok a, [b]) (Ok c, [""]) = (Ok a, [b])
concatP (Ok a, [""]) (Ok c, [""]) = (Ok a, [""])
concatP (Ok a, [b]) (Ok c, [d]) = (Ok a, [b++", "++d])
concatP _ _ = ((Error "Problem Concating"),[])
