# Chomsky NF prep

The most important question is whether a string is in the language
	If you have a deterministic PDA, you can just run the string through it 
	If it is not, then we need another way

To prep for Chomsky normal form (CNF), you have to change 2 things:
1. Remove lambda from all rules without changing the language made (Losing the empty string is okay)
2. Remove unit productions like X -> Y while still maintaining the effects 

![[Pasted image 20241104165252.png]]
In this, X goes to lambda so in S create a new rule s.t S -> ... | b (replace X with lambda)
Y can also go to lambda because Y -> X -> lambda so add new rule s.t X -> ... | aa (replace Y with lambda but make it its own rule, keep existing)
![[Pasted image 20241104165450.png]]
Now we can elim X -> Y since it is useless thus eliminating X altogether 
![[Pasted image 20241104165521.png]]
Then remove Y -> Y and then all that is left is Y-> B so subst that into S making S 
$S \rightarrow a | bb | aba | b | aa$ 

**Nullable**
A var is nullable if there is a rule s.t $X \rightarrow \lambda$ or if there is a rule s.t $Y\rightarrow X_{1}, X_{2}, ..., X_{n} \quad \forall X_{i}$ are nullable

So looking at our prev example, out null set is N = {X}, then we look for where X is on the right side (by itself) and then add it to our null set N= {X, Y} then we look for Y solely on right side and there is none so done


Another more visual way to do it is by substuting lambda in for the Null set 
![[Pasted image 20241104171508.png]]
But now this is diffierent from the original grammar
To use this method, go through each element in in N and then substitute {element | $\lambda$} (except for when the element appears by itself)
![[Pasted image 20241104171630.png]]

This gets
![[Pasted image 20241104171650.png]]
Which we can easily simplify

**EX 2**
![[Pasted image 20241104174333.png]]
In this, our nullable set is obiously Y
Thus making our new grammar: (remember ignore just lambda)
![[Pasted image 20241104174408.png]]
Here we can still see unit productions (X -> Y and Z-> Y and S->X|Y|Z)
This means you can replace the unit on the right hand with the values that X/Y/Z can be

The replacement paths you can take are *unit chains* so for this current one we have
S -> X -> Y
S -> Z -> Y
S -> Y
X -> Y
Z -> Y


unit closures are all the vars that you can reach from one so:
closure(S) = {X,Y,Z, S}
closure(X) = {X, Y}
closure(Z) = {Z, Y}
closure(Y) = {Y}

You can replace a unit with its non-unit replacements 
Going back to this,
![[Pasted image 20241104174408.png]]
you can replace X with aXb|ab|Y
NOTE: remove the null rules before unit rules
# Chomsky Normal Form
When grammar does not have a lambda and there are no unit productions, it is ready for CNF

Rules of 2 types:
1. $X \rightarrow c$ where $X \in V$ and $c \in \Sigma$
2. $X \rightarrow YZ$ where $X,Y,Z \in V$
Notice how it isn't recursive 

Note:
	RHS only has 1 non-empty 
<mark style="background: #ADCCFFA6;">	forgot stuff add here (from notes)</mark>


Going back to $a^{n}b^{n}$
$S \rightarrow aSb | \lambda$ 
Since the start var cant point to itself, we need to remove S to S
so
$S \rightarrow aXb | ab | \lambda$ 
$X \rightarrow aXb | ab$ 
Now the issue is that you can only have 1 letter on the right so add A -> a and B -> b
![[Pasted image 20241105082524.png]]
Now the issue is that AXB is not Normal form, only 2 elem from V on right hand side so introduce W
![[Pasted image 20241105082633.png]]
There can be different Ws like W -> XB
![[Pasted image 20241105082656.png]]


This shows 2 types of transformations:
1. when a terminal appears (lowercase letter) with length > 1, replace each terminal with unique non-terminal
	aXa becomes AXA with new rule A -> a
2. AFTER step 1, split the right hand sides of 2 vars and recursively introduce new vars mapping each substring until only 2 vars (like what we did in the last SS)


![[Pasted image 20241105083041.png]]

First step is to replace all lower case letters of length > 1 with their own rules so
X->a, Y->b, Z->c
![[Pasted image 20241105083159.png]]
The reason you don't replace c is because then you have a unit production which we don't want

Now we split the ones with length >2 to S->ACA/XAX and A -> XAX
So let W->AC and V-> XA

![[Pasted image 20241105083331.png]]
And now this is CNF!!


For quick recap, CNF has 2 forms:
1. right hand side is single terminal
2. ring hand side is 2 non-terminals
Now we can develop an algorithm for testing whether a grammar can generate a string
Referred to CYK algorithm
# CYK algorithm
Uses dynamic programming

Lets look at this grammar
![[Pasted image 20241105083942.png]]
Want to determine if baaa can be gen from this grammar
## Stage 1
Find all the vars that can make a substring of length one
So S-> XY cant make "a" or "b" because uses X and Y and each can give one
BUT X->a and X->b and Y->a and A->a
so they can be denoted like:
	b <- X
	a <- X,Y,A

## Stage 2
Now look at subst of length 2 that can be gen from what we found in stage 1
ba <- XX, XY, XA = X(A,X,Y)
aa <- XX,XY,XA,YX,YY,YA,AX,AY,AA = (A,X,Y)(A,X,Y)
<mark style="background: #FFF3A3A6;">I think we skip bb and ab because it isnt part of our goal string </mark>

Now we go back to our original grammar and remoe the impossible concatentations 
	So like all except XY, XA, AY is impossible
	ba <- XY, XA <- S, X
	aa <- XY, XA, AY <- S, X, Y
![[Pasted image 20241105083942.png]]

## Stage 3
Make subst of length 3
baa:
	b aa <- X(S,X,Y: from prev) = XS, XX, **XY** only one that is exists is S so <- S
	ba a <- (S, X)(X,Y,A) = XS, SY,SA,XX, **XY,XA** <- S,X
aaa:
	a aa <- (X,Y,A)(S,X,Y) = XS,XX,**XY**,YS,YX, YY, AS, AX, **AY**

![[Pasted image 20241105085517.png]]
(Underline is the valid ones)

## Stage 4
REpeat but:
	Since we looked at length 3, look at length 3 and length 1
	Since we looked at length 2, look at 2 of length 2

baaa:
	baa a <- (S,X)(X,Y,A) = XY, XA <- S,X
	ba aa <- (S,X)(S,X,Y) = XY <- S
	b aaa <- X(S,X,Y) = XY <- S

Goal is to see whether start var appears:
	Can stop after baa a because we see S
	If S is not available, then not genable



Now can the same algorithm be used to determine whether a CFL is empty or inf?
## Empty
Asking whether the grammar generates something that is nonempty
![[Pasted image 20241105090403.png]]
Pretty easy, pick a terminal rule (A->a) then sub all a for A
![[Pasted image 20241105090512.png]]
Now we can see that S->aa so it generates some string

### Ex2:
![[Pasted image 20241105090611.png]]

Replace all X with a
![[Pasted image 20241105090629.png]]
Now replace all Y with aa

![[Pasted image 20241105090641.png]]
Now we can see that the rule is aS so it will never end


## Same process for infinite
Checking if grammar is infinite
![[Pasted image 20241105090919.png]]
Pick a var to check for recursion and mark all instances (underline), then mark all fo the left hand side vars (rules) that include the marked var
![[Pasted image 20241105091103.png]]
Because A (the same one we marked initially) on the LHS is marked, then it is infinite

