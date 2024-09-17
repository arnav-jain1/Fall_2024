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
| rule              | desc                      |
| ----------------- | ------------------------ |
| r + s = s + r     | *union* is commutative    |
| (r+s)+t = r+(s+t) | Associative               |
| r + r = r         | Union is idempotent       |
| r + $\phi$ = r    | Union identity is $\phi$  |

## Character classes:
groups of letters in brackets represent set of any letter:
$[A-Za-z]$: Any char
$[A-Za-z]^{+} == [A-Za-z][A-Za-z]^{*}$ : Any char 1 or more times


For every Expression theres an NFA and therefore a DFA
and for every NFA/DFA, theres an expression 

# RegEx to NFA
How to prove that every Regex can be represented by an NFA/DFA
Base cases:
1. $\phi$, Can accept empty set {}
2. $\lambda$, can accept one string language/set
3. c, you can represent any combination of characters c in an alphabet $\sum\limits$ 
Now we have to represent the operations
1. Concatenation 
	Suppose NFA1 has two accepting states $NFA1_{x}$ and $NFA1_{y}$ 
	Then connect that starts of the NFA1 to the starts of NFA2 via lambda transitions
	The accepting state of the first one is no longer accepting
	The start of the second is no longer the start
2. Kleene star
	2 possibilities:
		1. Accept empty string if not already
			1a. If initial state of NFA is the accepting state, then done because already accepts empty string
			1b. If it is not an accepting state end
				1bi. No incoming edges from other states, turn it into accepting
				1bii. Incoming edges from other states, then create new accepting state with a lambda connecting the new accepting state with original start state
		1. Must allow restarts of NFA