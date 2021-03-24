data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
  -- fmap (a -> b) -> Expr a -> Expr b
  fmap f (Var x) = Var (f x)
  fmap f (Val n) = Val (f n)
  fmap f (Add (Expr y) (Expr z)) = Add (Expr (f y)) (Expr (f z))

