{-# LANGUAGE GADTs #-}

module ExpressionProblem where

import Data.Typeable (Typeable)

------

class Eval a where
  eval :: a -> Int

class Render a where
  render :: a -> String

-------

data Expr where
  Expr :: (IsExpr e) => e -> Expr
  deriving (Typeable)

-- This breaks the Expression Problem, as we need to add new operations into list of constraints here!
class (Typeable e, Eval e, Render e) => IsExpr e

instance Eval Expr where
  eval (Expr e) = eval e

instance Render Expr where
  render (Expr e) = render e

-------

data Val = Val Int
  deriving (Typeable)

instance IsExpr Val

instance Eval Val where
  eval (Val x) = x

instance Render Val where
  render (Val x) = show x

--------

data Add = Add Expr Expr
  deriving (Typeable)

instance IsExpr Add

instance Eval Add where
  eval (Add x y) = eval x + eval y

instance Render Add where
  render (Add x y) = "(" ++ render x ++ " + " ++ render y ++ ")"

add :: (IsExpr e1, IsExpr e2) => e1 -> e2 -> Add
add e1 e2 = Add (Expr e1) (Expr e2)

-------

main :: IO ()
main = do
  let expr = add (Val 42) (add (Val 314) (Val 217))
  putStrLn $ render expr
  putStrLn $ show $ eval expr
