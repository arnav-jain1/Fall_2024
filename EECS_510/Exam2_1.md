# Pushdown automata (PDA)
accept context free languages
	Diff between non-deterministic and deterministic
	Some CFL are inherently non-deterministic

![[Pasted image 20241017080540.png]]
A string in the PDA must end with an accepting state with an empty stack with only one symbol popped from the stack at a time
For the PDA with aabb:

| State | Input     | Stack     |
| ----- | --------- | --------- |
| $q_0$ | aabb      | $\lambda$ |
| $q_0$ | abb       | X         |
| $q_0$ | bb        | XX        |
| $q_1$ | b         | X         |
| $q_1$ | $\lambda$ | $\lambda$ |
Can also use symbols instead of table
$\vdash$  is a yield, indicates a step in the machine
$(q_{0}, aabb, \lambda) \vdash (q_{0}, abb, X) \vdash (q_{0}, bb, XX) \vdash (q_{1}, b, X) \vdash (q_{1}, \lambda, \lambda)$  
$\vdash^{*}$ means yields in multiple steps so it allows us to use shorthand
$(q_{0}, aabb, \lambda) \vdash^{*} (q_{1}, \lambda, \lambda)$  

SO the goal for PDA is always:
$(q_{0}, s, \lambda) \vdash^{*} (q_{f}, \lambda, \lambda)$ where s is start string and f is final state

## Definition
PDA is a finite state machine with a stack and has the following properties:
- Q= set of states, one of which is the start and a subset which is the final
- $\sum$ is the input alphabet
- $\Gamma$ is the stack alphabet (can include $\Sigma$) 
- $\delta$: $Q \times (\Sigma \cup \{\lambda\}) \times (\Gamma \cup \{\lambda\})$ : Finite subsets of Q and Gamma where given a state from and (optionally) a char from $\Sigma$ and (optionally) an char from stack $\Gamma$, determines the next state of the machine and the string from $\Gamma$ to put on the stack

![[Pasted image 20241017082551.png]]

### Example 2
Draw a PDA that accepts palindromes of as and bs with c in the middle that are odd
$\delta = \{((q_{0}, a, \lambda), (q_{0}, X)),\quad ((q_{0}, b, \lambda), (q_{0}, Y)), ((c, \}$ 
![[Pasted image 20241017083350.png]]


There are also ways to make PDAs more deterministic
![[Pasted image 20241017120722.png]]
First, use $ to denote the end of a language
![[Pasted image 20241017120742.png]]

So building abbaba$ would look like this

| State | Input     | Stack     |
| ----- | --------- | --------- |
| $q_0$ | abbaba$   | $\lambda$ |
| $q_1$ | abbaba$   | S         |
| $q_1$ | bbaba$    | XS        |
| $q_1$ | baba$     | S         |
| $q_1$ | aba$      | YS        |
| $q_1$ | ba$       | S         |
| $q_1$ | a$        | YS        |
| $q_1$ | $         | S         |
| $q_1$ | $\lambda$ | $\lambda$ |
![[Pasted image 20241017121357.png]]
The first 3 functions are unique but function 4 has same state, same input, with different stack and output (when compared to 1). Function 5 has same state as 3, can also take B, but has different output and stack.


## Definition
PDA is deterministic if for every $q \in Q$ (every state in set of states), for every $c \in \Sigma$ (every letter in the alphabet), and every $X \in \Gamma$ (every stack var in stack alphabet), there is exactly ONE of the following transitions
- $\delta(q,c,X)$ 
- $\delta(q,c,\lambda)$ 
- $\delta(q,\lambda,X)$ 
- $\delta(q,\lambda,\lambda)$ 
which makes sense because there the output of state, letter must be unique and then lambdas introduce non-determinism

## Summary
- Adding stack allows for more languages
- Known as context free languages (CFLs)
- Not all CFLs work with *deterministic* PDA 
- Using stack start and end of string can reduce non determinism (sometimes eliminate)

We know Formal grammar has symbols (ending and non-ending) as well as substituion (production) rules

# Context free grammar
## Definition
Formal grammar with:
- set of vars V with start variable
- Alphabet $\Sigma$ of terminal (ending) symbols
- Production rules in the form v->s where $v \in V$ and $s \in (V \cup \Sigma)^{*}$ (v is var and s is terminal and/or vars)

Example: CFG for $\{a^{n}b^{n} | n>0\}$ 
	Smallest string is ab so $$S \rightarrow ab$$
	since as come before bs, $$S \rightarrow ab \space| \space aSb$$
If the language was changed to $a^{n}b^{n}|n\ge0$ then:
	CFG becomes: $$S  \rightarrow \lambda \space | \space ab \space| \space aSb$$
	Which you can remove the ab from
Examle: Palindrome with c in the middle: 
	Similar: $S \rightarrow c \space | aSa \space | \space bSb$  


Notice that it goes outside in
Another example: CFG for all non palindromic
	![[Pasted image 20241018155832.png]]

Example: CFG for $L_{eq}= \{w | w \in (a+b)^{*} \wedge  n_{a}(w)==n_{b}(w)\}$  every a has b in any order
$$S \rightarrow aSbS | bSaS | \lambda$$
$S \Rightarrow bSaS \Rightarrow b\lambda aS \Rightarrow baS \Rightarrow baaSbS \Rightarrow baa\lambda bS \Rightarrow baab\lambda \Rightarrow baab$   
**Note that you only do one at a time, usually leftmost**

can also do this $$S \rightarrow SS | aSb | bSa | \lambda$$ 
## Simplifying
You can simplify a CFG $\iff$ 
	a rule is **NOT directly recursive**
	Has an unreachable state
	Has a non terminating state
Directly recursive: A var representing a rule appears in the right side (body of the rule)
Example:
$$
\begin{align} 
A\rightarrow a \space | \space aaA \space | \space abBc \\ B \rightarrow abbA \space | \space b \quad\quad\space\space\space
\end{align}$$
A is directly recursive because in the rule of A, A appears
B is not directly recursive because B does not appear meaning we can elimitate it
Starting with the easy one, 
$$A\rightarrow a \space | \space aaA \space | \space abBc \space | \space abbc$$
we can sub b in for B leaving abbA
then
$$A\rightarrow a \space | \space aaA \space | \space ababbAc \space | \space abbc$$
Another example of when to simplify is if you can never reach it. Simplify the following
![[Pasted image 20241028183042.png]]
Here B is unreachable so we can just remove it. Likewise, S is not directly recursive so we can eliminate it too so it becomes 
$A\rightarrow aA \space | \space \lambda$ or $S \rightarrow aS \space | \space \lambda$ which a* 

SImplify this
![[Pasted image 20241028183300.png]]
Notice that A will never end so we dont want that. the new grammar becomes
$S\rightarrow aSb \space | \space A \space | \space \lambda$ 



