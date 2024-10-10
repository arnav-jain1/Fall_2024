Closures are also values'
```
data FBAE where
	...
	Closure :: String -> FBAE -> Env -> FBAE
```
Lambda will now have a copy of the env when it was created (called a closure)
	Does not have abstract syntax

Now we are creating a new type called Val which will wrap the values
```
data FBAE where
	NumV :: Int-> FBAEVal
	ClosureV :: String -> FBAE -> Env -> FBAEVal

eval :: Env -> FBAE -> (Maybe FBAEVal)
```

This new closure will be used for static scoping
```haskell
eval e (Num n) = Just (NumV n)
eval e (Plus l r) = do { (NumV l') <- (eval e l) ; 
						 (NumV r') <- (eval e r) ;
						 just (NumV (l'+r'))
eval e (Lambda i s) = Just (Closure i s e)
eval e (App f a) = do
}
```
