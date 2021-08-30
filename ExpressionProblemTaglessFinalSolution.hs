{-# LANGUAGE RankNTypes #-}

module ExpressionProblemTaglessFinalSolution where

------ Data types ------

class Add e where
  add :: e -> e -> e

class Val e where
  val :: Int -> e

class Mul e where
  mul :: e -> e -> e

---- Operations -----

newtype Eval = Eval {eval :: Int}

instance Add Eval where
  add (Eval x) (Eval y) = Eval (x + y)

instance Val Eval where
  val = Eval

instance Mul Eval where
  mul (Eval x) (Eval y) = Eval (x * y)

-----

newtype Render = Render {render :: String}

instance Add Render where
  add (Render x) (Render y) = Render (concat ["(", x, " + ", y, ")"])

instance Val Render where
  val x = Render (show x)

instance Mul Render where
  mul (Render x) (Render y) = Render (concat ["(", x, " * ", y, ")"])

---- Usage ----

main :: IO ()
main = do
  let expr :: (Add e, Val e, Mul e) => e
      expr = add (val 42) (mul (val 314) (val 217))
  putStrLn $ render expr
  putStrLn $ show $ eval expr
  printAndEval expr

printAndEval :: (forall e. (Add e, Val e, Mul e) => e) -> IO ()
printAndEval expr = do
  putStrLn $ render expr -- <> " = " <> show (eval expr)
