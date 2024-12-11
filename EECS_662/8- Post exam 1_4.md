```haskell
type Store (Loc, Sto)
```

Loc is the next place to store memory while sto is the current memory

```haskell
derefStore (_, s) l = (s l)
```

```haskell 
initStore (0, initSto)
```
Initialize the memory, empty with the next val of 1 (can change to -1)


```haskell
setStore (m, s) l v = (m, setSto s l v)
```
Takes m, s (location and a store) and 

```haskell
newStore (l, s) = (l+1, s)
```

The big difference between store and env is that store persists outside of scope

```haskell 
data FBAEVal =
	...
	Loc :: Int -> FBAEVal
```
We make this its own class because we don't want to do math on them


New return value:
```haskell
type Retval = (FBAEVal, Store)
```


```haskell
type Retval = (FBAEVal, Store)

eval :: Env -> Store -> FBAE -> Maybe Retval
```
eval now takes an enviornment (normal), store (new, the "memory" with location and contents, empty at the start) and returns a retval (maybe, also new)

```haskell
eval e s (Seq l r) = do { (v, s') <- (eval e s l) ;
-- eval the left side which will return a value and a new state
						  (v', s'') <- (eval e s' r);
-- eval the right side with the new state to get another value and a new new state
						Just (v', s'')
-- return the new value and state
}
```


```haskell
eval e s (Plus l r) = do { ((NumV l'), s') <- (eval e s l) ;
-- eval the left side which will return a value and a new state
						   ((NumV r'), s'') <- (eval e s' r);
-- eval the right side with the new state to get another value and a new new state
							Just ((NumV  l'+r'), s'')
}
```


Dereferencing
```haskell
eval e s (Deref t) = do { ((Loc l'), s') <- eval e s t};
						  (derefStore (Loc l') s')}
```


New 
```haskell
eval e s (New t) = do { (v', (l, s')) <- eval e s t
		 Just ((Loc l+1), (setStore (newStore (l, s')) s' v' ))  }
```


# Vars
Identifiers don't change, variables do

We need variable declaration, deference, and assignment
We already have bind and new and that's all we really need
`var x:=t1 in t2` == `bind x= new t1 in t2`

Var is mutable while bind is not mutable
	Theoretically bind is not needed


## Var deref
`(deref (lookup x e) s)`
	Finds the var then gets the value in the state


## Assignment
`x := v == (set x v)`
x has to be a var, thats all

```
Asn :: FBAE -> FBAE -> FBAE
```



# Errors
Errors are a value, make it a new type
`eval _ bang = bang`


`x+y == eval x + eval y`
`== (bang "message") + 3`
but what if eval x returns a bang?

Then we want to return bang, so for every operation we need to make sure to return bang if any operands are bang


# Sums and products
Products (pair)
	(a,b) is `a and b` (like conjugation)
	Always 2 total functions (called Projection functions) 
		1: `(a,b)->a` 
		2: `(a,b)->b`
	Type is `A*B`
Sum (or a varient)
	`match x with (left a) -> s | (right b) -> t`
	read `a or b` 
	Injection functions take `a-> left a` and `b -> right b`
	type is `a+b` 

Algebraic type is a *sum* of products 
	Cause like every element (Plus, mult, bind, etc) is 1 or the other
	Products are like pieces of the constructs

Records ( like but not lists) are products
Case statements are sum


Records: `(1,(2,(3,4)))`
Trees: `(1, ((2, (3,4), (4, 5))`
Lists: `(1,(2,3,nil))` and `case x of left (v, L) | right nil`
<mark style="background: #FF5582A6;">^ this </mark>

### Concrete syntax 
- `(t1, t2)` builds a pair
```
bind p = (1,2) in
bind p = (3, p) in                  (3, (1,2))
bind p = (p, 4)                     ((3, (1, 2)), p)
```
- `fst` and `snd` to return the first and second element respectfully

```
bind p = (1,2) in
bind p = (3, p) in                  (3, (1,2))
bind p = (p, 4)                     ((3, (1, 2)), p)
```


```
t ::= (t,t) | fst t | snd t
v ::= (v,v)
T ::= T*T
```

#### Abstract syntax
3 new operations, 1 for creating 2 for accessing 
	`fst` and `snd` for accessing

```
Pair :: FBAE -> FBAE -> FBAE
Fst :: FBAE -> FBAE
Snd :: FBAE -> FBAE
```

eval
```
PairV :: FBAEVal -> FBAEVal -> FBAEVal
eval e :: env -> FBAE -> Maybe FBAEVal
eval e (Pair l r)= do {l' <- eval e l;
					   r' <- eval e r;
					   Just (PairV l' r')
					   }

eval Fst t = do { (PairV l _) <- eval e t;
				  Just l}
eval Snd t = do { (PairV _ r) <- eval e t;
				  Just r}
```


#### Sums
```
left 1
right "!"

match t with (left x) -> (f x) | (right x) -> print x
```

```
Left t :: (LeftV FBAEVal)
Right t :: (RightV FBAEVal)

Match :: FBAE -> FBAE -> FBAE
Match e (Match t l r) = do { t' eval e t; 
							   match t' with 
							   (left x) = eval e l 
							   (right x) = eval e r}
```

