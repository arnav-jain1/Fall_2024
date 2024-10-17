call by name vs call by value is on midterm
# Important defn
First order functions: Cannot take other funcs as arguments 
	C technically but func pointers exist
Higher order functions: Can take functions as args and can return functions
First class functions: Funcs are values 


# Optim
Optimizers are important because they speed up code significantly
```haskell
optimize :: BAE -> BAE
optimize (Plus l r) = if (optimize l) == (Nat 0) then (optimize r) else if (optimize r) == (Nat 0) then (optimize r) else (Plus (optimize l) (optimize r))
```
This looks at the left tree and right tree makes sure it isnt just 0
```
inc x = x + 3
inc 3
```
`x` is the formal parameter (like the param signature)
`x+3` is the body
`3` is the actual parameter
`inc 3` is an application of `inc` onto 3

# Functions (lambda)
## Concrete syntax
`lambda x in x + x`
	Function over **formal parameter** x
	Called a lambda or abstraction
	lambdas are values (can be passed into a function, assigned to a var, technically evaluates since it evals to itself)
	lambdas introduce a variable 
`(l a)`
	Applies a function `l` to formal parameter `a`
	Called an application or app of l (technically the space)
	Application (app) eliminate a variable
```
((lambda x in x+x) 3)
== 3 + 3
== 6
```
Note *very* similar to **bind**
```
((lambda x in x+x) 3) == (bind=3 in x+x)
```

```
FBAE ::= V|FBAE+FBAE|FBAE-FBAE
		| bind id = FBAE in FBAE
		| lambda id in FBAE ; Creates new identifiers and scopes
		| (FBAE FBAE)       ; Binds values to identifiers
		| id

V ::= Nat | lambda id in FBAE
```

```
(lambda x in x)
== (lambda x in x)

((lambda x in x) 5)
== [x<-5]x
== 5
```

```
((lambda z in z) (lambda x in x)) 
== [z->(lambda x in x)] z
== (lambda x in x)
```


```
((lambda x in (lambda y in x+y)) 3) 2)
== [x->3]((lambda y in x+y) 2)
== ((lambda in 3+y) 2)
== [y->2](3+y)
== 3+2 
== 5


(lambda x y z) == (lambda x in (lambda y in (lambda z in ...)))
```
Currying!!! 

```
((lambda x in (lambda y in x+y)) 1)
== (lambda y in 1+y)
```

```
((lambda x in (x 3)) (lambda z in z))
== [x->(lambda z in z)](x 3)
== (lambda z in z) 3
== [z->3]z
== 3
```

```
bind inc=(lambda x in x+1) in (inc 3) [(inc,(lambda x in x+1))]
== ((lambda x in x + 1) 3) 
== 3+1
== 4
```

```
bind inc=(lambda x in x+1) in             
	bind dec=(lambda x in x-1) in
		bind sqr=(lambda x in x*x) in
			(inc (sqr (sqr 3)))
== ((lambda x in x +1) (sqr (sqr 3)))
== ((lambda x in x +1) (lambda x in x*x in (lambda x in x*x in 3)))
== ((lambda x in x +1) (lambda x in x*x in (3 * 3)))
== ((lambda x in x +1) (lambda x in x*x in 9))
== ((lambda x in x +1) 9*9)
== ((lambda x in x +1) 81)
== 81 + 1
== 82


Context
[(inc, (lambda x in x+1))]
[(dec, (lambda x in x-1)), (inc, (lambda x in x+1))]
[(sqr, (lambda x in x*x)), (dec, (lambda x in x-1)), (inc, (lambda x in x+1))]
```
This is kind of like making a library of functions that can be called in the body

$\lambda x . s$ = lambda x in s

## Abstract syntax
```haskell
data FBAE where
  | Lambda :: String -> FBAE -> FBAE
  | App :: FBAE -> FBAE -> FBAE
  | Id :: String -> FBAE
  | Bind :: String -> FBAE -> FBAE
```

## Inference rules

$$\frac{f\Downarrow (\lambda i . s) \quad a \Downarrow v_{a} \quad [i \rightarrow v_{a}]s\Downarrow v_{s}}{(f\space a) \Downarrow v_{s}}\text{Beta (function)}$$
$$\frac{a \Downarrow v_{a} \quad [i \rightarrow v_{a}]s \Downarrow v_{s}}{\text{bind i = a in s} \Downarrow v_{s} }\text{Bind}$$ 
<mark style="background: #FF5582A6;">Ask about the difference </mark>


F evaluates to a lambda in the body
a evaluates to va
Substitute va for i in the body s which evals to vs


This is strict (call by value)
<mark style="background: #FF5582A6;">Substitute a for va</mark>


lambda is VERY similar to bind (derived form)
```
bind i=a in s == ((lambda i in s) a)
```

<mark style="background: #FFF3A3A6;">IMPORTANT (MIDTERM FINAL)</mark>
Derived form:
	Define a new concrete syntax
	Define what the new syntax means in terms of existing language constructs
	Safe way to extend new langs
	Like GCC building itself

# Church's lambda calculus
(Basis of any functional language)

Can represent any computable function 
	Equivalent to turing machnines (Not proven but essentially)

Turing machines are states (*imperative programming*)
Lambda Calculus is functions (*Functional programming*)

Turing machines won and are used for everything (Von Neumann) because RAM is state and you look for stuff in that state

Concrete syntax
```
LC ::= | id
	   | lambda id in LC
	   | (LC LC)

v ::= lambda id in LC
```
$$\frac{f\Downarrow (\lambda i . s) \quad [i \rightarrow a]s\Downarrow v_{s}}{(f\space a) \Downarrow v_{s}}\text{Beta (function)}$$
This is similar but it is lazy eval/call by name because it evaluates a at run time
	Not really common cause it can mess up order

Can now do any computable function!! 


### Examples:
```
(lambda x in x)
```
is this a value yes or no? Yes!

```
(lambda y in y)(lambda x in x)
== [y->(lambda x in x)y ]
== lambda x in x

f= lambda y in y
s= y
i= y
va = a = lambda x in x

```

```
(lambda x in x x) 3
== [x->3](x x)
== (3 3)
== !
```
3 applied to 3 didnt work

```
(lambda x in x x)(lambda y in y)
== [x->lambda y in y](x x)
== (lambda y in y)(lambda y in y)
== [y->lambda y in y]y
== lambda y in y
```
but this did because magic


No numbers in lambda calculus

```
(lambda x in x x)(lambda y in y y)
== [x ->lambda y in y y](x x)
== (lambda y in y y)(lambda y in y y)
== (lambda y in y y)(lambda y in y y) 
```
woah that's a loop!
Also called $\Omega$ combinator

## making it (Call by val)
```haskell 
evalM Lambda i s = Just (Lambda i s)
evalM (Id _) = Nothing
evalM App f a = do { (Lambda i s) <-  evalM f;
					 va <- eval< a;
					 evalM (subst va i s) }
evalM Bind i a s = do { va <- evalM a
						evalM (subst va i s) }



evalM Bind i a s = evalM (App (Lambda i s) a)
```
Call by value means you evaluate it before you pass it in
Call by name you straight up pass the param
so like
```python
test(1+1,1)
```
for call by value you pass 2
For call by name you pass 1+1
### Call by name
```haskell
evalM Lambda i s = Just (Lambda i s)
evalM (Id _) = Nothing
evalM App f a = do { (Lambda i s) <- evalM f;
					 evalM (subst a i s) }
evalM Bind i a s = evalM (subst (evalM a) i s)
```

```
ones = 1:ones
```
Good for modeling things that run forever (OS, clock) but only possible with call by name
### Small examples:
```
bind f = (lambda x in x) in (f 2)
== [f->lambda x in x] (f 2)
== (lambda x in x) 2
== [x->2]x
== 2
```

```
bind f = (lambda x in x) in (f 2+1)
== [f->lambda x in x] (f 2+1)
== (lambda x in x) (2+1)
-- call by name
== [x->2+1]x
== 2+1
== 3
-- call by value
== (lambda x in x) (3)
== [x->3]x
== 3
```

```
bind n = 1 in 
	bind f = (lambda x in x +n) in 
		bind n = 2 in
			f 1
== bind f = (lambda x in x + 1) in
	bind n = 2 in 
		f 1	
== bind n = 2 in
	(lambda x in x + 1) 1
== (lambda x in x + 1) 1
== 2
```


Note: (f a) -> (App f a)

```haskell
Env = [(String, FBAE)]

eval e (Lambda i s) = (Lambda i s)
-- intuition comes from bind and app being similar for below
eval e (App f a) = do {(Lambda i s) <- eval e f; 
						va <- eval e a;
						eval (i, va):e s; }
-- Looks for i in current env, if found will return the value otherwise will return Nothing
eval e (Id i) = (lookup i e)
```
**Lambda is a value**


```
bind n = 1 in                       [(n, 1)]
	bind f = (lambda x in x + n) in [f, (lambda x in x+n), (n,1)]
		bind n = 2 in               [(n, 2), (f, (lambda x in x+n)),(n,1)]
			f 1                     

== (lambda x in x + n) 1            [(n, 2), (f, (lambda x in x+n)),(n,1)]
== x+n                          [(x,1),(n, 2),(f,(lambda x in x+n)),(n,1)]
== 1+n                              [(n, 2), (f, (lambda x in x+n)),(n,1)]
== 1+2                              [(f, (lambda x in x+n)),(n,1)]
== 3
```
BUT THIS IS WRONG (because 2 got subst for where it shouldnt have)

## Scoping
Static scoping: Scope where lambda is *defined*, correct way
Dynamic scoping: Scope where lambda is *used*
```
bind n = 1 in                       -- defined here for static
	bind f = (lambda x in x + n) in 
		bind n = 2 in               
			f 1                     -- defined here for dynamic

```

```
bind n = 1 in                                                     [(n, 1)]
	bind f = (lambda x in x + n) in  [f, (lambda x in x+n) [(n,1)], (n,1)]
		bind n = 2 in      [(n, 2), (f, (lambda x in x+n) [(n,1)]), (n,1)]
			f 1                     

== (lambda x in x + n [(n, 1)]) 1
== x+n               [(x, 1),(n, 2),(f, (lambda x in x+n) [(n,1)]), (n,1)]
== 1+1
```

Watch backaroo bonsai

Every question is 5 pts

Except tf 15 or so

Differed vs Immediate subst, types of vars


((Lambda i s) a) == bind i a s