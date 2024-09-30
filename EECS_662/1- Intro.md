# Prelude
## Datatypes
```haskell
data IntList where
	Null :: IntList
	Cons :: Int -> IntList -> IntList
```
Can you understand this?
	In this, IntList is the name of the datatype while Null and Cons are the constructor? 
	IntList is a list of Ints that ends in Null
```haskell
Null
Cons 1 Null
Cons 1 (Cons 2 Null)
```
These are all values of IntList

This also makes a recursive framework. If you want to get the size:
```haskell
size :: IntList
	Null = 0
	Cons x xs :: 1 + size xs
```
What does this do?
	Base case is Null, this marks the end so return 0
	Cons x xs takes a list input and splits it into two parts, the first element and the rest. Then add 1 with recurse of the rest
```haskell
mystery :: IntList
	Null = 0
	Cons x xs :: x + mystery xs
```
This sums all the elements

This recursion is very useful and will be used a lot
## Do block
Haskell do allows code to be executed in order in **monads**
```haskell 
do {
	a <- f x;
	b <- g y;
	return (a + b)
}
```
For this, both f and g are monadic functions 
for example:
``` haskell 
f x = if x > 3 then Just x else Nothing
g x = if x < 4 then Nothing else Just x
```
Then if we run 
```haskell
test1 = do {
	a <- f 4;
	b <- g a;
	return (a + b)
}
test1

== Just 8
```
but if either a or b evaluate to nothing, then test1 will be Nothing
# Notation
## Expressions
Concrete syntax will be haskell style but without the highlighting
## Meta language
The language above the language, defines aspects of languages that we are going to study

### Grammars
Defined with `::=` and `|` (for alternatives). Followed by a symbol in all caps which is considered a variable 
```
t ::= NUM
	| t+t
	| t-t
	| if t then t else t
```
Left of the ::= is the meta var defined by the right 

### Inference rules
Look proofs, if you know the top part then you know the bottom part
$$\frac{A,B}{A +B}$$
If you know A and B, then you know A + B.
Numerator is antecedent and denominator is consequent 

Infrence rule without antecedent (numerator) is an axiom
$$\frac{}{A}$$
A is always true
### Derivations 
COmbine multiple inference rules by using consequent of one and antecedent of another.
So like this 
$$\frac{\frac{A\wedge B}{B}\;\frac{A\wedge B}{A}}{B\wedge A}$$
This means that $A\wedge B \iff B\wedge A$ 

# Intro
When making a language, we need to describe what each syntax means. You have to know what the language is supposed to do before we can test it
- Define concrete syntax of new language
- Define the meaning of each syntactic element
	- Evaluation semantics tells us what it does (execution)
	- Static semantics tells us what we can predict (type checking)
3 ways of doing this 
- denotational: map each language structure to a mathematical function 
- operational: Define how legal strings in a language are evaluated
- Axiomatic: Define pre- and post- conditions on execution of language constructs

# Compilers and interpreters 
## Language processing (exam)
Two styles: Compiler and interpreter
- Compiler turns code into an executable form (binary)
	- Source language is the language being translated
	- Target language is the language being translated to
- Interpreters define a function that executes the language directly. It is not put into another form but executed directly
	- Embedded language is the language being interpreted 
	- Host language is the known language that defines the interpreter
### Concrete syntax
It is like a set of strings that are defined by the grammar
Grammar rules are how strings/tokens are interpreted 
Term is a string that is defined by the grammar 
Language is a set such that 
$$L = \{s : string | G(s))\}$$ where G are the grammar rules


## Defining syntax
*Programs are data structures*
Define syntax using syntax rules
Simple arithmitic expression
```haskell
AE ::= num
	| AE + AE
	| AE - AE
	| (AE)
```
AE is an infinite set
Examples:
```haskell
4
1 + 3
(2 + 2) + (5 - 7)
1 + 3 - (5 + (8 - 4))
```
Non-examples:
```
+ 4
3 * 2
)(
A + V
```
Concrete syntax: what programmers write, strings (boring)
abstract syntax: what interpreters operate over- the data structure! 

```haskell
data AE where
	Num :: Int -> AE
	Plus :: AE -> AE -> AE
	Minus :: AE -> AE -> AE
	deriving (Show, Eq)

(Num 3)
(Plus (Num 4) (Num 7)); 4 + 7
```

- AE = type name
- Num, plus, minus = Constructors, make elements of type
- AE -> AE: signature 
- Defines all constructors 

parser: concrete syntax -> abstract syntax 
`parse 1+3 == (Plus (Num 1) (Num 3))`

# Interpreters
Interpreter takes a language and maps it to a value:
$$ E : L \rightarrow V$$
Where E is an interpreter, L is our language, and V is our value
Values cannot be evaluated further (cant be broken up more)

Abstract syntax is the data structure that represents terms (AST)


Simplest syntax
```haskell
AE ::= num
```
Abstract syntax: 
```haskell 
data AE where
	Nat :: Int -> AE
	(deriving Eq,Show)
```
(deriving Eq, Show) = gets the eq and show functions so that you can compare and print stuff
Nat contructor
```haskell
eval :: AE -> Int
```
Parser translate concrete syntax into AST
```haskell
parse "1" == (Nat 1)
parse "2" == (Nat 2)
parse "a" == !
parse "1+2" == !
```
`!` = bang
`?` = hook
`*` = splat
`#!` = shebang

Interpreter/evaluator will translate AE into values
```haskell
eval::AE-> Int
eval (Nat x) = x
```
Put it all together
```haskell 
interp x = eval (parse x)

interp "1" == 1
interp "3" == 3
```
OR 
```haskell
interp = eval . parse
```
Adding addition
``` haskell
data AE where
	Nat :: Int -> AE
	Plus :: AE -> AE -> AE
	(deriving Eq, Show)

eval::AE->Int
eval (Nat x) = x
eval (Plus x y) = (eval x) + (eval y)

eval (Plus (Nat 1) (Nat 3)) 
== (eval (Nat 1)) + (eval (Nat 1))
== 1 + 3
== 4
```
Do programs in AE Terminate? Yes
Do programs ever crash? No
cant do much tho 


``` haskell
data AE where
	Nat :: Int -> AE
	Plus :: AE -> AE -> AE
	(deriving Eq, Show)

eval::AE->Int
eval (Nat x) = x
eval (Plus x y) = (eval x) + (eval y)
eval (Minus l r) = let x = (eval  l) - (eval r) in 
						if x <0 then error "!" else x
```
You can also use a *magic number* which is a number indicating there is being a problem without ending the problem.
	You can set the magic value to -1 so it returns that instead of error but it sucks because it is hard to tell what is going on
This is helped by *maybe*
```haskell
data Maybe A =
	Just :: A -> Maybe A
	Nothing :: Maybe A
```

## Monad
Using Maybe
```haskell
eval::AE -> Maybe Int
eval (Nat x) = Just x
eval (Plus l r) = do { x <- eval l;
					   y <- eval r;
					   Just (x + y) }
eval (Minus l r) do { x <- eval l;
					  y <- eval r;
					  if x >= y then Just (x - y) else Nothing }
```

x `<-` is called a bind and we bind the output of the right side to the left. Only works if it is a monad
Can also replace Just with return

We can change the signature to `eval::AE -> Maybe AE` if we want to return any type (like bools) instead of just ints

```haskell
eval::AE -> Maybe AE
eval (Nat x) = Just (Nat x)
eval (Plus l r) = do { (Nat x) <- eval l;
					   (Nat y) <- eval r;
					   Just (Nat (x + y)) }
eval (Minus l r) do { (Nat x) <- eval l;
					  (Nat y) <- eval r;
					  if x >= y then Just (Nat(x - y)) else Nothing }
```
; is a sequence operator which means do it in order.


### Language properties
wff - Well formed formula ("woof")
Completeness - every wff that we put into eval will get evaluated
Determinicity - every wff we put into eval will produce only one value
Normalizing- every wff we put into eval will terminate in a value
Value- A good computation result

## Adding booleans
Def of ABE is AE with Bools abstract syntax
```
ABE ::= Nat | ABE + ABE | ABE - ABE | (ABE)
		| true | false | if ABE then ABE else ABE
		| ABE <= ABE | ABE && ABE | isZero ABE

v ::= Nat | true | false
```

# Inference Rules and Axioms
Axioms: Things we know, given
Inference Rules: Things we can deduce from what we know in 1 step
Derivations: Sequences of inference rule applications

An inference rule is a set of antecedents and a consequence. If the antecedents are true, the consequents follow immediately
If we know A and we know B, then we know C  (a and b antecedents, C is consequence). Antecedent is numerator and consequence is denominator
$$
\frac{A \space \space B}{C} = \text{Inference rules}
$$
Axiom is an inference rule with no antecedent 

Syntax:
$$\frac{t_{1} \in L \space\space t_{2} \in L}{t_{1}-t_{2} \in L}$$
if 2 elements are in L, then their subtraction is in L
$$ \frac{A \land B}{B}$$
If A and B are true, then B is true

##### Note Notational convention
v is a var for values
t is for terms 
$\pminus$ is an operation in concrete syntax while + is haskell 
$t_{1}\Downarrow t_{2}$ t1 evaluates to t2

`eval (Num v) = v` a number v in my language evalutes to v no matter what
	Good because this is the basecase
Addition in AE is addition in haskell:
$$\frac{t_{1}\Downarrow v_{1} \space \space t_{2} \Downarrow v_{2}}{t_{1}\pm t_{2} \Downarrow {v_{1} \pm v_{2}} }$$
if t1 evals to v1 and t2 evals v2, then my t1+t2 evaluates to v1+v2
$$\frac{t_{1}\Downarrow v_{1} \space \space t_{2} \Downarrow v_{2} \space\space v_{1} \ge v_{2}}{t_{1} - t_{2} \Downarrow {v_{1} - v_{2}} }$$
But this is incomplete!
$$\frac{t_{1}\Downarrow v_{1} \space \space t_{2} \Downarrow v_{2} \space\space v_{1} \lt v_{2}}{t_{1} - t_{2} \Downarrow ! }$$
Since these two are complements, one or the other will always be used

