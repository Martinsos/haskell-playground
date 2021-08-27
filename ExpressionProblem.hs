module ExpressionProblem where

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y

render :: Expr -> String
render (Val x) = show x
render (Add x y) = "(" ++ render x ++ " + " ++ render y ++ ")"

main :: IO ()
main = do
  let expr = Add (Val 42) (Add (Val 314) (Val 217))
  putStrLn $ render expr
  putStrLn $ show $ eval expr
