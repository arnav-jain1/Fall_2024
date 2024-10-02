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
$$\frac{f\Downarrow (\lambda i . s) \quad a \Downarrow v_{a} \quad [i \rightarrow v_{a}]s\Downarrow v_{s}}{(f\space a) \Downarrow v_{s}}$$
This is strict (call by value)
<mark style="background: #FF5582A6;">Substitute a for va</mark>
VERY similar to bind (derived form)
```
bind i=a in s == ((lambda i in s) a)
```

Derived form:
	Define a new concrete syntax
	Define what the new syntax means in terms of existing language constructs
	Safe way to extend new langs
	Like GCC building itself
# Important defn
First order functions: Cannot take other funcs as arguments 
	C technically but func pointers exist
Higher order functions: Can take functions as args and can return functions
First class functions: Funcs are values 

