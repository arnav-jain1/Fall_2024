How to show language

1. Concrete syntax (t ::= ...)
2. Inference rules
3. Abstract syntax (the code)
4. Interpreter
and then typechecker


Bind creates a binding between an identifier and a value (let)
```haskell
bind x=5 in
	bind y=6 in x + y
== bind y=6 in 5+y
== 5+6
==11
```
the `in` is the scope of the variable like curly braces

Inner workings
```haskell
bind x=5 in
	bind y=6 in x + x
== bind y=6 in 5+5
== 5+5
==10
```
Example 2
```haskell
bind x=5 in
	bind x=6 in x + x
== bind x=6 in x + x
== 6+6
==12
```
if redefined, do not substitute 
Example 3
```haskell
bind x=5 in
== bind x=6+x in x + x
== (6+5) + (6+5)
== (11) + (11)
== 22
```
if redefined, do not substitute *other than the redefinition*

```haskell
bind x=5 in 
	x + bind y=6 in x+y
== 5 + bind y=6 in 5+y
== 5 + 5 + 6
== 5 + 11
== 16
```


```haskell
bind x=5 in 
	x + bind x=6 in x+x
== 5 + bind x=6 in x + x
== 5 + 6 + 6
== 5 + 12
== 17
```
x hasn't been redefined yet

## Concrete syntax
```
BAE ::= num
	| BAE + BAE
	| BAE + BAE
```

Instance: Occurance of identifier 
	Literally everywhere you see x/any var
Binding instance: Where an identifier is declared and given a value
	when you set x/var to a value
Bound value: the value given to an identifier in a binding instance
	Var's val
Scope: Where the identifier is defined and can be used
	{} in C, 'in' in haskell
Bound instance: Where the identifier was used in scope
	Use of var
free instance: Where identifier was used outside of scope
```other
bind x = ->x+5
	in x-4
```

The scope is everything ***after*** the in

## Inference rules for bind and identifiers
$[x\rightarrow v]t$ = replaces all free instances of $x$ in $t$ with $v$

- $[x \rightarrow 5]3$ == 3 (replace all x in 3 with 5)
- $[x \rightarrow 5]x$ == 5 (replace all x in x with 5)
- $[x \rightarrow 5]5 + 5$ == 1- (replace all x in 5+5 with 5)
- $[x \rightarrow 7]\text{bind x=7 in bind y=5 in x + y}$  == $\text{bind x=7 in bind y=5 in x + y}$ Since x is bound, dont replace
- $[x \rightarrow 5]\text{x + bind x=10 in x}$ == 5 + bind x = 10

Inference rule for bind
$$\frac{a \Downarrow v_{a}\qquad [i \rightarrow v_{a}]s\Downarrow v_{s}}{\text{bind i = a in s} \Downarrow v_{s}}\text{BindE}$$ 
assume a = $v_{a}$ 
Substitute $v_{a}$ for i in the body of bind which evaluates to $v_{s}$
So the result is $v_{s}$ 

Inference Rules Identifiers 
$$\frac{}{x \Downarrow \bot}\text{IDE}$$
- Evaluating an identifier means the identifier was not replaced
<mark style="background: #FF5582A6;">- No bind defined the indentifier of it would no longer be there</mark>
```
bind x = 3 in y
== y
== BOOM

Parsed to by abstract syntax below to
Bind (("x") (Nat 3) (Id "y") ) 
Identifier, value, scope
```

Abstract syntax
```haskell
data AE where 
	Nat :: Int -> AE
	ID :: String -> AE
	Plus :: AE->AE->AE
	Minus :: AE->AE->AE
	Bind :: String -> AE -> AE -> AE
	-- String is the name of the identifier, first AE is the value, second is the scope
	deriving (Show, Eq)
```


Evaluation
```haskell
eval::AE -> Maybe AE
eval (Nat x) = Just (Nat x)
eval (Id s) = Nothing
eval (Plus l r) = do { (Nat x) <- eval l;
					   (Nat y) <- eval r;
					   Just (Nat (x + y)) }
eval (Minus l r) do { (Nat x) <- eval l;
					  (Nat y) <- eval r;
					  if x >= y then Just (Nat(x - y)) else Nothing }
eval (Bind i a b) do { v <- eval a; 
						b' <- (subst i v b);
						(eval b')}
```
For this we need to subst
```haskell
subst :: String -> BAE -> BAE -> BAE
subst x v (Nat x) = (Nat x)
-- Below if x the var is equal to x' in the scope, return v. if it is different then return the previous thing
subst x v (Id x') = if x=x' then v else (Id x')
subst x v (Plus l r) = (Plus (subst x v l) (subst x v r))
subst x v (Plus l r) = (Minus (subst x v l) (subst x v r))
-- Plus and minus same way because if there is an issue its in the arguments
subst x v (Bind x' v' t') = if x=x' then 
							(Bind x' (subst x v v') t') 
							else
							(Bind x' (subst x v v') (subst x v t')) 
```

The issue with this is that you walk through all of your code down and back up with each bind
This is a reference interpreter, it does what the code is supposed to do, but is not necessarily how to do it 

Old way 
```
eval bind x = 5 in 
		bind y = 6 in 
			x + y
== bind y=6 in 5+y
== 5+6
== 11
```
New
```
eval bind x = 5 in       [("x", 5)]
		bind y = 6 in    [("y", 6), ("x", 5)]
			x + y        [("y", 6), ("x", 5)]
== 5 + 6
== 11
```
Environment: list of vars and respective values in the scope

Bind and plus are fundamentally different
	Bind manipulates environment while plus manipulates vars in an environment

 ```haskell
	 eval (Bind "x" (Num 5)              [("x", (Num 5))]
		 (Bind "y" (Num 6)               [("y", (Num 6)), ("x", (Num 5))]
			 (Plus (Id "x") Id ("y"))))  [("y", (Num 6)), ("x", (Num 5))]
== (Plus (Num 5) (Num 6))
== 5 + 6
== 11
```
 
 ```haskell
	 eval (Bind "x" (Num 5)              [("x", (Num 5))]
		 (Bind "x" (Num 6)               [("x", (Num 6)), ("x", (Num 5))]
			 (Plus (Id "x") Id ("x"))))  [("x", (Num 6)), ("x", (Num 5))]
== (Plus (Num 6) (Num 6))
== 6 + 6
== 12
```

The environment is a *stack* so make sure you push the variables 

 ```haskell
 eval (Bind "x" (Num 5)              [("x", (Num 5))]
	 (Bind "x" (Num 6)               [("x", (Num 6)), ("x", (Num 5))]
		 (Plus (Num 6) Id ("y"))))  Error!
== (Plus (Num 6) (Num 6))
== Nothing
```

```haskell 
eval (Bind "x" (Num 5)                     [("x", (Num 5))]
	(Plus (ID "x")                         [("x", (Num 5))]
			(Bind "x" (Num 6)              [("x", (Num 6))("x", (Num 5))] 
				(Plus (Id "x") (Id "x")))))
== (Plus (Num 5) (Plus (Num 5) (Num 6)))
== (Plus (Num 5) 12)
== 17
```

```haskell
eval (Bind "x" (Num 5) 
		(Plus                            [("x", (Num 5))]
			(Bind "x" (Num 6)            [("x", (Num 6)), ("x", (Num 5))]
				(Plus (Id "x") (Id "x")) [("x", (Num 6)), ("x", (Num 5))]
			(Id "x")
== 
eval (Bind "x" (Num 5) 
		(Plus 12 (Id "x")))              [("x", (Num 5))]
== 17
```

No more new inference rules because logic wasn't changed just speed was

To test, give all possible values OR do an inductive proof

 This is how you make an environment, this says env will be a list of pairs of strings and BAE
Takes something of type A, pairs of As,Bs and returns Maybe type of B
x:xs adds x to the front of xs. Pattern matches with non empty list 
 ```haskell
Type Env = [(String, BAE)]
lookup :: A -> [(A,B)] -> Maybe B
x:xs
```
New eval

```haskell 
eval :: Env -> BAE -> Maybe BAE
eval _ (Num n) = Just (Num n)
eval e (Plus l r) = do { (num l') <- (eval e l) ; 
						 (num r') <- (eval e r) ;
						 Just (l' +  r')
					   }
eval e (Minus l r) = do { (num l') <- (eval e l) ; 
						 (num r') <- (eval e r) ;
						 if r' <= l' then Just (l' -r') else Nothing
					   }
eval e (Bind x a s) = do { a' <- eval e a; 
						   eval (x,a'):e s;
					   }

```