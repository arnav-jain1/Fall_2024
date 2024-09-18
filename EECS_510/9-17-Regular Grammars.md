Examples of regex over $\sum\limits = \{a.b\}$ 

Strings ending in ab: (a+b)\*ab
Strings that begin and end with different symbols: a(a+b)\*b + b(a+b)\*a
Strings of even length: ((a+b)(a+b))\*
strings with even number of as: (ab\*a + b)*

# Precedence 
1. Grouping (parenthesis)
2. Kleene star (exponentiation)
3. Followed by concatenation (mult)
4. Then Union (addition)

Distributive law exists 
$$a(a+b) = aa +ab$$
and 
$$ b + ba = b(\lambda + a)$$
not commutative when distributing $$a(a+b) \neq (a+b)a$$
may look like mult, but actually concatenation
$a^{3}$ works and is just aaa but $a^{n}$ where n is a var is not a regex because unknown

## Rules
| rule                        | desc                         |
| --------------------------- | -------------------------- |
| r + s = s + r               | *union* is commuta           |
| (r+s)+t = r+(s+t)           | Assoc                        |
| r + r = r                   | Union is id                  |
| r + $\phi$ = r              | Union identity   s $   hi$   |
| (rs)t = r(st)               | Concat is      c      e      |
| $\lambda$r = r$\lambda$ = r | Concat iden  ity is  lambda  |
| r$\phi$ = $\phi$r = $\phi                                  |
| r(s+t)=rs + rt                                             |
| $(r^{*})^  Kleene star is idempotent idempotent idempotent |

## Character classes:
groups of letters in brackets represent set of any letter:
$[A-Za-z]$: Any char
$[A-Za-z]^{+} == [A-Za-z][A-Za-z]^{*}$ : Any char 1 or more times


Kleene's Thm:
	For every Expression theres an NFA and therefore a DFA
	and for every NFA/DFA, theres an expression 

# RegEx to NFA
How to prove that every Regex can be represented by an NFA/DFA
Base cases:
1. $\phi$, Can accept empty set {}
2. $\lambda$, can accept one string language/set
3. c, you can represent any combination of characters c in an alphabet $\sum\limits$ 
![[Pasted image 20240917161716.png]]
Now we have to represent the operations
1. Concatenation 
	Suppose NFA1 has two accepting states $NFA1_{x}$ and $NFA1_{y}$ 
	Then connect that starts of the NFA1 to the starts of NFA2 via lambda transitions
	The accepting state of the first one is no longer accepting
	The start of the second is no longer the start
![[Pasted image 20240917161754.png]]
1. Kleene star
	2 possibilities to incorporate empty string:
		1. Accept empty string if not already
			1a. If initial state of NFA is the accepting state, then done because already accepts empty string
			1b. If it is not an accepting state end
				1bi. No incoming edges from other states, turn it into accepting
				1bii. Incoming edges from other states, then create new accepting state with a lambda connecting the new accepting state with original start state
				![[Pasted image 20240917162556.png]]
		2. Must allow restarts of NFA
		![[Pasted image 20240917162601.png]]
2. Union: Imagine there are 2 separate NFAs, NFA1 and NFA2. Then to combine it, create another state (q0) that connects to each of them with a lambda thus giving us a choice
	![[Pasted image 20240917162739.png]]

## NFA to regex
Let there be an automata
![[Pasted image 20240917163123.png]]
This represents (expr1)\*(expr2)(expr3)\*
NOTE: The NFA must have these properties
1. No edges other than self loop entering initial state
2. No edges other than self loops leaving the final state
3. Only one final state and final state $\neq$ initial state
4. No jail state (just remove it if its there)
Convert this
![[Pasted image 20240917163402.png]]
First check the rules
1. Yes, need to fix
2. Yes, need to fix
3. Check!
4. Check!
To fix 1 and 2, turn it into an NFA with lambdas everywhere (lambda start to q0 and lambda from q1 to end)
![[Pasted image 20240917163527.png]]
Then you want to put it into a similar shape as the one above while accounting for all paths

Tip: The number of paths through each node = incoming edges * outgoing edges (disregard loops) so for q0 2 * 1 = 2 and for q1 2 * 2 = 4

| path<br>                                    | expr  |
| ------------------------------------------- | ----- |
| $q_{s} \rightarrow q_{0} \rightarrow q_{1}$ | a\*b  |
| $q_{1} \rightarrow q_{0} \rightarrow q_{1}$ | aa\*b |
The next step is to eliminate q0 by making a path from qs to q1 that contains a\*b and then adding aa\*b to the original self loop (this keeps the original functionality of b\* while also adding the q1 -> q0 -> q1) loop
![[Pasted image 20240917164438.png]]
So the table is now 1 * 1 = 1

| path                                        | expr           |
| ------------------------------------------- | -------------- |
| $q_{s} \rightarrow q_{1} \rightarrow q_{f}$ | a\*b(b+aa\*b)* |
This simplifies down to (a+b)\*b aka must end in a b

state-bypass and elimination technique: Recording all paths and then eleminating the node associated with it (what we just did)