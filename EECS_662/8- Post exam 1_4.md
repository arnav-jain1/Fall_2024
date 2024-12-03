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