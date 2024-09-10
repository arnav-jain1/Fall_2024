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
$$\frac{a \Downarrow v_{a}\qquad [i \rightarrow v_{a}s\Downarrow v_{s}]}{\text{bind i = a in s} \Downarrow v_{s}}$$
