# Derivation Trees
Tree representation for derivations of strings in CFLs
For example: Tree for structure of sentence for the English language
![[Pasted image 20241028183842.png]]


## Another more mathy example:
This grammar is a math equation with +, \*,  and ().
$$ \langle exp\rangle = \langle exp\rangle +\langle exp\rangle |\langle exp\rangle * \langle exp\rangle | (\langle exp\rangle) | a | b | c$$
To derive a + b * c:
	$\langle exp\rangle \Rightarrow \langle exp\rangle + \langle exp\rangle$ 
	$\quad\quad\space \Rightarrow a + \langle exp\rangle$ 
	$\quad\quad\space \Rightarrow a + \langle exp\rangle * \langle exp\rangle$ 
	$\quad\quad\space \Rightarrow a + b * \langle exp\rangle$ 
	$\quad\quad\space \Rightarrow a + b * c$ 
![[Pasted image 20241028184526.png]]	
Leftmost derivation:
	$\langle exp\rangle \Rightarrow \langle exp\rangle + \langle exp\rangle$ 
	$\quad\quad\space \Rightarrow \langle exp\rangle * c$ 
	$\quad\quad\space \Rightarrow \langle exp\rangle + \langle exp\rangle * c$ 
	$\quad\quad\space \Rightarrow \langle exp\rangle + b * c$ 
	$\quad\quad\space \Rightarrow a + b * c$ 
![[Pasted image 20241028185148.png]]	
Ambigious grammar: A grammar where there are multiple derivation trees for the string

Is there a "correct" one?

From a math perspective, the first one because order of ops
	The first one says that the equation is the sum of two subequations where the first one is a and the second one is b * c

Its a little weird to thing about but higher precendence operations like mult happen further down

Recreating the expression so that there is less ambiguity and the addition is at the top
![[Pasted image 20241028185910.png]]


![[Pasted image 20241028190322.png]]

This is also *left-recursion* because the recursion is always happening on the left side 

Right recursion is also a thing which is right to left associativity
![[Pasted image 20241028190718.png]]
This will recurse right to left


Expression trees used instead of deriv trees because easier
Changes:
	Collapse all chains of single var subst
	Contains only leaves of terminals
	Easier to process and read
![[Pasted image 20241028190823.png]]

Prefix notation: Pre order traversal (left to right)
	curr -> left -> right

so pre order for this is:
	+ a \* b ^ c ^ d + e f
To eval, we go right to looking for operator so:
	+ a (\* b (^ c (^ d (+ e f))))
so c^(d^(e + f)) * b + a
so if a = 3, f= 1, rest = 2:
	2^(2^(2+1))\*2 + 3 = 2^8\*2+3=515
	or 
	+ 3 (\* 2 (^ 2 (^ 2 (+ 1 2))))
	+ 3 (\* 2 (^ 2 (^ 2 3)))
	+ 3 (\* 2 (^ 2 8))
	+ 3 (\* 2 256)
	+ 3 512
	= 515

Same for postfix
Post fix: Postorder tree traversal (left right curr) then reverse (or traverse clockwise)
	+ * ^ ^ + e f d c b a
	so
	a b c d f e + ^ ^ * +

Same vars:
	3 2 2 2 1 2 + ^ ^ * +
	3 2 2 2 3 ^ ^ * +
	3 2 2 8 ^ * +
	3 2 256 * +
	3 512 +
	515

This is important because we want languages to be:
	Explicit
	Simplified
	Unambiguious 
The more "defined" the easier it is to read


# Equivalence of PDA and CFG
Just like RegEx shows finite automata, PDAs and CFGs describe CFLs
	Can construct one from the other and the meaning of the lang remains the same

## CFGs to PDAs (simple)
Non-deterministic PDA (NDPA) simulates rules of CFG
1. PGA has 2 states, final state and non-accepting start state
2. Transition from start to end that pushes CFG start var on stack
3. For every rule v -> r where v is a var in grammar and r is v's replacement, place a self loop lambda, v-> r
4. For every terminal symbol c add a self loop on final state: c, c->lambda

Example: $L_{eq}$
$$S \rightarrow SS \space | \space aSb \space \space | \space bSa \space | \space \lambda$$
1. create the 2 states
![[Pasted image 20241028193540.png]]
2. Single transition between the 2
![[Pasted image 20241028193609.png]]
3. Add loop for rules
![[Pasted image 20241028193728.png]]
4. Add loop for terminals
![[Pasted image 20241028193743.png]] 

Resulting table for trying to make baaabb
![[Pasted image 20241028193808.png]]

## PDA to CFGs (single state)
Convert single state PDA to CFG 
1. Put new non terminating start state that pushes a unique var S onto the stack and moves to the original state (Like inverse of 1)
2. Add new transition $\lambda, S \rightarrow \lambda$ onto second state (loop)
3. Any transition that pops $\lambda$, replace it with transition that includes popping/pushing our stack alphabet $\Gamma$ so $c,\lambda\rightarrow r$ becomes $c, V\rightarrow rV \quad \forall V \in \Gamma$ 
4. For each transition $c,V\rightarrow R$ on final state, add grammar rule $V \rightarrow cr$
Example: $L_{eq}$
![[Pasted image 20241028194753.png]]
1. Add Non accepting start state, push S
![[Pasted image 20241028194836.png]]
2. Add $\lambda, S \rightarrow \lambda$ to second state
![[Pasted image 20241028194915.png]]
3. replace transitions that pop lambda to those that pop and push every elem of stack alphabet
so $a,\lambda \rightarrow X$ is replaced with 
	$a,S\rightarrow XS$
	$a,X\rightarrow XX$
	$a,Y\rightarrow XY$
so $b,\lambda \rightarrow Y$ is replaced with 
	$b,S\rightarrow YS$
	$b,Y\rightarrow YY$
	$b,X\rightarrow YX$
![[Pasted image 20241028195215.png]]

4. Lastly for each transition $c,V\rightarrow r$ add $V \rightarrow cr$
So 
$$S \rightarrow aXS \space | \space bYS \space | \space \lambda$$
$$X \rightarrow aXX \space | \space bYX \space | \space b$$
$$Y \rightarrow bYY \space | \space aXY \space | \space a$$
## PDA to CFG General case 
Stack trace of aaababbbbbaa:
![[Pasted image 20241029121107.png]]
How long were vars in the stack? 

Stack var lifetime: The duration they were in the stack as well as their first and last appearance
![[Pasted image 20241029121503.png]]
Notice $X_{1}$, it appears and then 3 different Xs go through their lifetime and then X is gone. The 3 Xs $(X_{2}, X_{3}, X_{4})$ are the aftereffects of X

The symbol $\langle pVq \rangle$ are actions of PDA that go from state p where V is at the top of the stack (in this case X1) to q after 1+ transitions such that V and all aftereffects are gone

This is important because these symbols will be the vars of the resulting grammar 
	They show all the steps it takes to remove and add vars to a stack


At step 3, the stack is $X_{3}, X_{2}, X_{1}$ 
To show the next step in pVq form:
	Consume the first b with transition b, X -> $\lambda$  which takes is from state $q_{0} \rightarrow q_{0}$ since it is a self loop and the new resulting string abbbbbaa
	The transition can be written as $(q_{0}, babbbbbaa, X_{3}X_{2}X_{1}) \vdash (q_{0}, abbbbbaa, X_{2}X_{1})$ 
	The pVq form for X3 is then $q_{0}X_{3}q_{0}$ 

To get X1 and steps 1-7:
	$(q_{0}, babbbbbaa, X_{1}) \vdash^{*} (q_{0}, bbaa, \lambda)$ 
	$q_{0}X_{1}q_{0}$ 

In summary the 2 we have are:
![[Pasted image 20241029200956.png]]
For referene, the first symbol means we start at q0, consume a b, and push X3 but resume end at q0 with the stack unchanged from when it was pushed
The second symbol means: start at q0, consume aababbb, push X1 but then end at q0 with the stack being the same as where we started with

Now look back at the first grammar rule $a, \lambda \rightarrow X$ which consumes an a, pops nothing from the stack and adds an X while staying at q0


