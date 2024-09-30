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
	lambdas are values
	lambdas introduce a variable 
`(l a)`
	Applies a function `l` to formal parameter a
	Called an application or app of l
	Application (app) elimitates a variable

# Important defn
First order functions: Cannot take other funcs as arguments 
	C technically but func pointers exist
Higher order functions: Can take functions as args and can return functions
First class functions: Funcs are values 
