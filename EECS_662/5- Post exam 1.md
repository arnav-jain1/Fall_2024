Closures are also values (kind of)
How it is used:
```
bind n = 1 in                       [(n,1)]
	bind f = (lambda x in x + n)    [(f, (lambda x in x+n) [(n+1)]) (n,1)]
		bind n = 2 in
			f 1
== 
```
As you can see the current env is saved
```
data FBAE where
	...
	Closure :: String -> FBAE -> Env -> FBAE
```
Lambda will now have a copy of the env when it was created (called a closure)
	Does not have abstract syntax

Currently, our evaluater: `eval :: Env->FBAE->Maybe FBAE`
Now we are creating a new type called Val which will wrap the values
```
data FBAEVal where
	NumV :: Int-> FBAEVal
	ClosureV :: String -> FBAE -> Env -> FBAEVal

eval :: Env -> FBAE -> (Maybe FBAEVal)
```
It will now be `eval::Env->FBAE-> Maybe FBAEVal`

This new closure will be used for static scoping
```haskell
eval e (Num n) = Just (NumV n)
eval e (Plus l r) = do { (NumV l') <- (eval e l) ; 
						 (NumV r') <- (eval e r) ;
						 just (NumV (l'+r'))
eval e (Lambda i s) = Just (Closure i s e)
eval e (App f a) = do {(CloseureV i s e') <- eval e f);
						a' <- (eval e a);
						eval ((i, a'):e') s
}
You use e' and the new value here cause when youre inside the body of the function you dont care about the rest
```
**Definitions:**
	Immediate subst $\Rightarrow$ static scoping
	Deferred subst $\Rightarrow$ Dynamic scoping
	deferred subst + enclosures $\Rightarrow$ static scoping (fast and good yay!)

# Derived form
Programs have a basic core, that core is used to build the rest of the language

```
unless ...
1. unless c e                                   Concrete Syntax
2.  Unless BBAE -> BBAE -> BBAE,                Abstract syntax
3. unless c e == (same as) if c then false else e

```

$$\frac{e\Rightarrow False \quad e\Downarrow v}{\text{unless}\space c \space e \Downarrow False} \text{UnlessF}$$
$$\frac{e\Rightarrow False \quad e\Downarrow v}{\text{unless}\space c \space e \Downarrow False} \text{UnlessF}$$
