Set returns the original value

```
bind m = new 5 in                [(m, ->5)]
	bind n = m in                [(n, ->5)]
		set m = 6 ; deref n      
== 6
```
m will store the address of 5, n is the same as m so it also stores the address of 5
They *point* to the same place
When m is set to 6 and n is referenced, the value is 6


```
bind inc = (lambda in (set l ((deref l) + 1)))
	bind n = new 5
		inc n; deref n
==
```
Note that inc does not return a value 

Use sto to store vars at a specific location
Location is an address (Int)
```haskell
type Sto = Loc -> Maybe FBAEVal

type Loc = Int
```
Sto is a *maybe* because NULL pointers


Dereferencing 
```haskell
derefSto s l = (s l)
```

Initial store stores nothing
```haskell
initSto :: Sto
initSto x = Nothing
```
initSto for any val is Nothing always, because it initializes it (like Null ptr)

Updating the store:
- Adding a value to location 3
	`m0 = \l -> if l=3 then 1 else (initSto l) 
	If you call m0 3 you get one, otherwise you get Nothing
	This is akin to storing 1 at location 3 which is why asking for something other than 3 gets you nothing
- Adding another location
	- `m1 = \l -> if l=1 then 2 else (m0 l)`
	This creates another block of memory at location 1 which holds 2, else look at the prev location
	if we call m1 3 then we get 1 back, if we call m1 on 2
- Location 3
	- `m2 = \l -> if l=2 then 0 else (m1 l)`
- To replace values what we can do is 
	- `m3 = \l -> if l=3 then 4 else (m2 l)`
	What this does is essentially makes the previous l=3 state unreachable

Memory is a random access structure (not FIFO or LIFO) that you can put things in, get things out, and replace things

Not exactly memory but close enough (if it walks like a duck, talks like a duck...)
This is like a linked list model, you can also use a looooong array which would be faster but harder to implement (need to initialize)

```haskell
type Store (Loc,Sto)

-- setSto :: Sto -> Loc  -> FBAEVal -> Sto
```
Store is a pair that where Loc is the next memory and Sto is the current memory 

```haskell
derefStore (_,s) l = s l
```
We don't care about the next location only the current one, this dereferences

```haskell
initStore = (0, initSto)
```
Initial memory where the current is nothing (ignore the 0 for now)

```haskell
setStore (m,s) l v = (m, setSto s l v)
```
This sets the value

```haskell
newStore (l, s) = (l+1, s)
```
This keeps track of the last memory location and we increment it so we don't delete our current. This is why it was 0 because the initial is 0

l just keeps track of the next memory location 


This is different than the env because the changes to the store stay even after the function call

```haskell
data FBAEVal
	Loc :: Int -> FBAEVal
```
If Num is a location, it becomes hard to figure out whether it is a location of a Num

Locations can be calculated, stored, etc which is easier with Ints too
Also now, the left and right branch order of Plus matters


```haskell
type Retval = (FBAEVal, Store)
```
This will return something which will help us keep track if something goes good

```haskell
eval :: Env -> Store -> FBAE -> Maybe Retval
```


```haskell
eval e s (Seq l r) = do {(v,s') <- (eval e s l);
						 (v', s'') <- (eval e s' r)
						 Just (v', s'')
}
```
This will help with stuff like x:=x+1 ; x:=x+1
	Doing one thing and then the other
The original value is gone (could make it an underscore)
This is why order is important

```haskell
eval e s (Seq l r) = do {(v,s') <- (eval e s t);
						 (setStore s' l v)
}
```
This will overwrite the original value

The do clause is capturing the state and then returning the val associated with it

```haskell
eval e s (Plus l r) = do { ((NumV l'),s') <- (eval e s l);
						   ((NumV r'), s'') <- (eval e s' r);
						   Just ((NumV l'+r'), s'')

}
```
This new idiom is sequential programming because we are modifying the state and calculating as per that