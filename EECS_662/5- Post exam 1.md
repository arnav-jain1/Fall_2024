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

$$\frac{e\Downarrow False \quad e\Downarrow v}{\text{unless}\space c \space e \Downarrow v} \text{UnlessF}$$
$$\frac{e\Downarrow True}{\text{unless}\space c \space e \Downarrow False} \text{UnlessT}$$
If there is a value other than true or false, it would stall. There is no rule for what happens. It wouldn't error, it would just stall

```haskell
eval Unless c e = do { (BoolV c') <- eval c;
					   if c' == False then (Just eval c) else (Just BoolV 
															   false)
or
eval Unless c e = do
}
```
$$\frac{\Gamma \vdash c: TBool \quad \Gamma \vdash e:TBool}{\text{unless} \space c \space e : TBool }$$

## Elaboration
1. Define concrete syntax
2. Define abstract syntax
3. *Define elaboration function*
4. Define Type rules
5. Extend type inference 


elab :: Embedded language AST -> Host Language AST

To eval translated embedded language to host lang and execute as usual:
```haskell
evale t = eval [] (elab t)

or

evale = eval [] . elab
```


Lets start with increment (++)
This is the host language 
```haskell
data FAE where
 |
```
FBAE without the bind

This is the embedded language 
```haskell 
data FAEX where

```

Adding conc syntax of inc function
```
inc x = x + 1
```
Inference rule 
$$\frac{x \Downarrow v -1}{\text{inc} \space x \Downarrow x}$$

Elaborator:
```haskell
elab :: FAEX -> FAE
elab NumX n = Num n
elab PlusX l r = (Plus (elab l) (elab r))
elab LambdaX i b = (Lambda i (elab b))
elab AppX f a = (App (elab f) (elab a))
elab IdX s = Id s
elab IncX x = (Plus (elab x) (Num 1))
```

Type checking:
```haskell
typeofX t = typeof [] (elab t) 
```
The issue with this is that it will produce errors for our embedded language. For example: getting assembly language for C which is obv wrong so we have to make a new type checker


# Recursion
## Dynamic scoping

```haskell
bind fact = 
	lambda x in 
		if x=0 then 1 else x * (fact x-1) [(fact, (lamnda x in x...))]
	fact 3
== (lambda x in if x=0 then 1 else x * (fact x-1)) 3
			[(fact, (lambda...))]
== if x=0 then 1 else x * (fact x-1)
			[(x, 3), (fact, (lambda...))]
== if 3=0 then 1 else 3 * (fact 3-1)) 
			[(x, 3), (fact, (lambda...))]
== 3 * (fact 2)  
			[(x, 3), (fact, lambda...)]
== 3 * (lambda x in if x=0 then 1 else x * (fact x-1))) 2  
			[(x, 3), (fact, lambda...)]
== 3 * if x=0 then 1 else x * (fact x-1) 
			[(x, 2), (x, 3), (fact, lambda...)]
== 3 * if 2=0 then 1 else 2 * (fact 2-1) 
			[(x, 2), (x, 3), (fact, lambda...)]
== 3 * 2 * (fact 1)
			[(x, 2), (x, 3), (fact, lambda...)]
== 3 * 2 *	(lambda x in if x=0 then 1 else x * (fact x-1)) 1
			[(x, 2), (x, 3), (fact, lambda...)]
== 3 * 2 *if x=0 then 1 else x * (fact x-1) 
			[(x, 1), (x, 2), (x, 3), (fact, lambda...)]
...
== 3 * 2 * 1 * (fact 0)
== 3 * 2 * 1 * 1
== 6
			[(fact, (lambda...))] 
-- Becaus when your recursion completes it gets popped
```

## Static scoping

```haskell
bind fact = 
	lambda x in 
		if x=0 then 1 else x * (fact x-1) [(fact, (lamnda x in x...))]
	fact 0
== (fact 0) 
		[(fact, closure x ... [])]
== (closure x ... []) 0
== if x = 0 then 1 else ... [(x, 0)]
...
== 1

	fact 1
== (fact 1) 
		[(fact, closure x ... [])]
== (closure x ... []) 1
== if x = 0 then 1 else ... 1 
		[(x, 1)]
== if 1 = 0 then 1 else 1 * (fact 0)
		[(x, 1)]
== 1 * (fact 0) [(x, 1)]
-- BUT FACT IS GONE NOOO
-- PSYCH, just pull the lambda that had the closure into the new env

	fact 1
== (fact 1) 
		[(fact, closure x ... [(fact, (closure ... [same]))])]
== (closure x ... [(fact ...)]) 1
== if x = 0 then 1 else ... 1 
		[(x, 1), (fact, closure ... [])]
== if 1 = 0 then 1 else 1 * (fact 0)
		[(x, 1), (fact, closure ... [])]
== 1 * (fact 0) [(x, 1)]
		[(x, 1), (fact, closure ... [])]

```

## Omega
### Dynamic
```
bind o = lambda x in x x
o o 
== (lambda x in x x) (lambda x in x x)
== x x 
		[(x, (lambda x in x x))]
== (lambda x in x x) (lambda x in x x) 
		[(x, (lambda x in x x)), (x, (lambda x in x x))]
```

### Static
```
bind o = lambda x in x x
o o 
== o o 
== [(o, (closure x in x x []))]
== (closure x in x x []) (closure x in x x [])  
			[(o, (closure x in x x []))]
== x x
			[(x, (closure x in x x []))]
== (closure x in x x []) (closure x in x x [])
			[(x, (closure x in x x []))]
```
Here x is the function and the argument


## Y comb
Gives us (a little too much) recursion
	Dynamic and static scoping
And doesnt do anything


The Y comb does something (does a func) and has an off switch
```
bind Y = (lambda f (lambda x in (f (x x)))
				   (lambda x in (f (x x)))) 
	in (Y F)
F is the work that we want to do
== (Y F)           [(Y, ...)]
== (lambda f (lambda x in (f (x x))) (lambda x in (f (x x))))    []
== (lambda x in (f (x x))) (lambda x in (f (x x)))         [(f, F)]
== (lambda x in (F (x x)) (lambda x in (F (x x))))               []
== (F (x x))                             [(x, lambda x in F (x x))]
== F (lambda x in (F (x x)) (lambda x in (F (x x))))               
		[(x, lambda x in F (x x))]
== (F (F (x x)))                [(x, lambda x in F (x x))]
== (F (F (F (x x))))            [(x, lambda x in F (x x))]
== (F (F (F (F (x x)))))        [(x, lambda x in F (x x))]
...
```
We are doing a recursive function without any name!!

Still doesnt terminate or do anything

- `f` is the function being called recursively 
- F is the function that we are *applying* doesn't have a name
```
F = (lambda z in if z=0 then z else z + g (z - 1))

g is F for now (cheating a lil) bc F isnt defined yet
F = (lambda g in (lambda z in if z=0 then z else z + g (z - 1)))
```
<mark style="background: #FF5582A6;">Ask why ^</mark>
```
== (lambda z in if z=0 then z else z + (F (z-1)))
== (lambda z in if z=0 then z else z + ((lambda z in if z=0 then z else z + (F (z-1)))(z-1)))
```


Putting it in the y comb
```
bind F = (lambda g in (lambda z in if z=0 then z else z + (g (z-1)))) in
	bind Y = (lambda f (lambda x in (f (x x)))
						(lambda x in (f (x x))))
	in ((Y F) 5)

== (lambda x in (F (x x))) (lambda x in (F (x x)))) 5
== (F (x x)) [(x, (lambda x in (F (x x))))]
```
<mark style="background: #FF5582A6;">Aaaaand Im lost</mark>



# Typing functions
```
bind inc = lambda x in x + 1
 (inc inc)
== (inc inc) [(inc, lambda x ...)]
== (lambda x in x+1) (lambda x in x + 1)    
		[(inc, lambda x ...)]
== x + 1    
		[(x, lambda x...(inc, lambda x ...)]
== (lambda x in x + 1) + 1
```
this is not a value cant do lambda + Int because TYPES

```
Eval:
bind x=1 in           [(x,1)]
	x+1
== 1 + 1

Type check:
bind x=1 in           [(x,TNum)]
	x+1
== TNum + TNum
== TNum
```

```
```