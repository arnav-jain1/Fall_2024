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
initSto for any val is Nothing always

Updating the store:
- Adding a value to location 3
	`m0 = \l -> if l=3 then 1 else (initSto l) 
	If you call m0 one 3 you get one, otherwise you get Nothing
- Adding another location
	- `m1 = \l -> if l=1 then 2 else (m0 1)`
- Location 3
	- `m2 = \l -> if l=2 then 0 else (m1 1)`
	