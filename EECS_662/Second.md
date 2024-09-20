Define eval:

`eval` == `eval [("pi", (Num 3))]`

`eval:: Env -> BAE -> Maybe BAE`
Eval is a function that takes in an Env and then outputs a function that takes in a BAE and outputs a Maybe BAE
	Currying takes in one arguement and outputs a function that takes in one function and outputs one ...  which is similar to but not the same as a function that takes in 2 and outputs one

```haskell
eval :: Env-> BAE -> Maybe BAE
eval _ (Num n) = Just (Num n)
eval e (Plus l r) = do { (Num l') <- eval e l;
						 (Num r') <- eval e r;
						 Just (l'+r')
					   }
eval e (Minus l r) = do { (Num l') <- eval e l;
						  (Num r') <- eval e r;
						  if r'<= l' then Just (l'+r') else Nothing
					    }
eval e (Bind x a s) = do { a' <- eval e a;
						   eval (x,a'):e s;
}
eval e (Id x) = (lookup x e)
```
This takes the value of a and the string x and then adds it to the enviornment and then evaluates 

This is called deferred substitution 
	There is no more "walking the code" meaning you don't go up and down a lot you just do one pass through
	No more subst

Our old eval did direct subst and let that be evals
New uses deferred subst and called that eval 

Show correctness by 
$$\forall t: BAE \quad eval \space t = evals \space t$$
This says that our for all t in BAE, eval t is the same as the old eval(s) of t
We can prove this by induction of Num and Id

# Predicted behavior (type checking)
Starting with a tiny language predicting odd/even:
```haskell
data EOVal where 
	| Even | Odd

predict :: [("String", EOVal)] -> BAE -> Maybe EOVal
predict _ (Nat n) = Just (if even n then Even else Odd)
predict c (Plus l r) = do { l' <- predict c l;
							r' <- predict c r;
							Just (if l' == r' then Even else Odd) }
predict c (Bind x a s) = do {a' <- predict ca;
							 predict (x:a'):c s}
predict c (Id x) = (lookup x c)
```
Predict has *abstract interpretation* so that it can make predictions without running the code itself

```haskell
data BAE where
	| Nat :: Int -> BAE
	| Boolean :: Bool -> BAE
	| Plus :: BAE -> BAE -> BAE
	| And :: BAE -> BAE -> BAE 
	| If :: BAE -> BAE -> BAE -> BAE
	| Bind :: String -> BAE -> BAE -> BAE
	| Id :: String -> BAE
```
Evaluator:
```haskell
eval Env -> BAE -> Maybe BAE
eval e (Nat n) = Just Nat n
eval e (Boolean b) = Just Boolean b
eval e (Plus l r) = do { Nat l' <- eval e l; 
						 Nat r' <- eval e r;
						 Just (Nat l' + r') }
eval e (And l r) = do { (Boolean l') <- eval e l;
						(Boolean r') <- eval e r;
						Just (Boolean (l' && r;))}
eval e (If e t f) = do { (Boolean c') <- eval e c;
						 (if c' then eval e t else eval e f) }
eval e (Bind i a b) = do { a' <- eval e a; -- Same as before
						   eval (i:a'):e b }
eval e (Id x) = (lookup e x) -- Same as before
```

Instead of odd even lets do type Num and Type bool
```haskell
data TBAE where 
	| TNum | TBool | 
```
Calculating whether the term is a num or bool
```haskell 
predict :: [(String, TVal)] -> BAE -> Maybe TVal
predict _ (Nat _) = return TNum
predict g (Plus l r) = do {TNum <- predict g l;
						   TNum <- predict g r;
						   Just TNum}
predict g (And l r) = do {TBool <- predict g l;
						  TBool <- predict g r;
						   Just TBool}
predict g (If e t f) do {t' <- predict g t}

}
```