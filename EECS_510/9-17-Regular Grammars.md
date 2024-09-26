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

# NFA to grammar 
![[Pasted image 20240920100710.png]]
Take this NFA and change q0 -> S, q1 -> X, q2 -> Y, and q3 -> Z
What would the grammar be for it? 
S -> 0S | 1S | 1X
X -> 0Y | 1Y 
Y -> 0Z | 1Z
Z -> $\lambda$ 
so generating 11101 it would be 
S -> 1S -> 11S -> 111X -> 1110Y -> 11101Z -> 11101

This is called **right linear grammar** where there is at most ONE var on the right side of the expression and is the rightmost symbol (ex 0S, S is right most and there is only 1)

Process:
	1. Remove all jail states
	2. For each transition, make a grammer rule like so: 
		1. Place the "from" var on the left side
		2. Make a string on the right by adding the char output and then the state var
	3. For the accepting state, include $\lambda$

You can also turn the grammer back into automata
You can also have >1 char 

Gramamr:
	S -> aX | bY | $\lambda$ 
	X -> aS | bZ 
	Y -> aZ | bS
	z -> aY | bX
respective dfa
![[Pasted image 20240920132325.png]]


## Left Linear grammar
Generate the strings backwards (right to left) so you have to traverse the machine backwards

Final states:
	Accepting state becomes final state and vice versa
	Multiple accepting states become 1 via lamda

![[Pasted image 20240920133027.png]]
Y will be start and S will be end
Y -> Xa
X -> Saa | Xb
S -> $\lambda$ 
You can simplify it and remove the S entirely 

Generate aabbba
Y -> Xa -> Xba -> Xbba -> Xbbba -> Saabbba (or just aabbba) -> aabbba


## Converting left to right
Right grammar
C -> Bb
B -> Aa
A -> Bab | $\lambda$ 

So the first step is to convert this into N/DFA

A has the lambda that means it's the start state 
Nothing has C meaning its the final state
![[Pasted image 20240920134210.png]]
Then we can convert this into a left linear grammar
A -> aB
B -> bC | abA
C -> $\lambda$ (again C optional)
You ***cannot*** mix the rules


Make a left Linear for missing one of the letters of the alphabet
	((a+b)* + (b+c)* + (a+c)\*)
First draw the NFA
![[Pasted image 20240920135252.png]]
Then 
Make it so there is only one accepting state
![[Pasted image 20240920135309.png]]
From there, write the grammar
F ->X$\lambda$ | Y$\lambda$ | Z$\lambda$ | W$\lambda$
Y -> Yb | Ya | Xa | Xb
Z -> Za | Zc | Xa | Xc
W -> Wb | Wc | Xb | Xc
X -> $\lambda$

Which simplifies to: 

F -> $\lambda$ | Y | Z | W
Y -> Yb | Ya | a | b
Z -> Za | Zc | a | c
W -> Wb | Wc | b | c

# How do we know if the lang is a "regular lang"
Must be able to:
	preserve the "regularness" of a language (???)
	Develop algorithms for answering questions about regular languages
	Show that non regular languages exist

A set is closed under an operation if when the result of the operation being applied to any two elements remains in the set

Need to know when this property does not hold

Regular expressions are easy, either they are empty ($\phi$) or infinite with the Kleene Star

Automata is more complex
1. Convert to a regex
	1. If it accepts anything its not empty
	2. If there is a * then it's infinite
2. Eye ball it
	1. depth-first traversal
	2. Look for path from start to accepting
	3. Language is empty if there is no accepting
3. Brute force
	1. Try all possibilities (computer)

Cycle:
![[Pasted image 20240920153907.png]]
4 possible states so p=4
The machine wants strings that are multiples of 4 (4n)
A singular cycle is $(a + b)^{4}$ 

The number of states in a path is |string| + 1 (extra cause of start state)
so if the string is "aaaa" then the number of states touched is 5 and since p is 4, then 5>p so there must have been at lewast one cycle

To check for emptyness we need to check for strings of length 0 to p 

##### Def
To determine by brute force if a DFA can accept any string, try all possible strings in $\sum^{*}$ (the alphabet)  within the range\[0,p] (determines emptyness)

Let there be a regular language L that accepts at least one string

If a DFA accepts a string length of >=p, we know there has to be a cycle in the accepting path meaning the language is infinite. 
Since no cycle can have more than p states, we only need to test lengths from \[p,2p-1]. If there is a string in this range, then the language is infinite

##### Def
To determine by brute force if the language of the finite automation is finite, test all strings in $\sum^{+}$ with lengths in \[p,2p-1] for acceptance 

Let there be a regular language L that accepts some string s
and 
$$|s| \ge p$$
where p is the number of states in the minimized DFA

By the pigeonhole principal, we know that there must be a cycle in the accepting path. 


Let the cycle be called y
Let the stuff before the cycle be called x
Let the stuff after the cycle be called z
So then the string s = xyz (z can be empty)

Let the language be ab(ba)\*b
x = ab
y = ba
z = varies but must end in b

| **String** | **Partitions** | z           | **String length** |
| ---------- | -------------- | ----------- | ----------------- |
| abb        | xb             | b           | 3                 |
| ab(ba)b    | xyb            | b           | 5                 |
| ab(baba)b  | $xy^{2}b$      | $y^{b}=bab$ | 7                 |
![[Pasted image 20240924200841.png]]
Note: 
	ab _ b is not looped
	All strings except abb is length of 5 or more
	if string length is at least p (3), there is a cycle
	$|xy| \le 5$  

## Def Pumping theorem
Pumping theorem: For any infinite language *L* there is a positive number *p* such that for every string *s* where $s \in L$ and $|s| \ge p$ we can write the string as $s=xyz$  and the following hold:
	$|y| > 0$ 
	$|xy| \le p$  
	$xy^{*}z \in L$ 
- p = # states in DFA
- y is non empty because the cycle has at least one edge
- Kleene star on y in 3rd point means we can chose y 0 to inf number of times and still reach accepting state

Why is pumpability important? 
	Helps prove whether language is regular or not
	If infinite language does not satisfy the theorem, it is not regular (Proves irregular not regular)
	Only needs one string to prove irregularity

Example
Show $a^{n}b^{n}$ is not regular
Well for ab you need 3 states, for aabb you need 5, for aaabbb you need 7, etc.
So a machine with finite number of states cannot display this expression but we need to prove this 

##### Proof
Split $a^{p}b^{p}$ into 3 parts xyz (we dont know what p is but some properties must be true)
- $|xy| \le p$ 
- Must know y must appear within the first p symbols (because it represents a cycle)
- y = aS
Pumping y:
- Number of aS changes 
- Number of bS does not
- Not part of the language therefore not regular

##### better way
Steps:
1. Suppose L is regular
2. know that pumping constant *p* exists for L
3. Chose a string in the language *w*
4. Look at all decompositions of w into xyz so that
	1. $|xy| \le p$
	2. $|y| \ge 1$ 
5. Chose i so that $xy^{i}z \notin L$ 

Let L be regular therefore some p exists for L
Let $w = a^{n}b^{n}$ 
$x=a^{j}\quad y=a^{k} \quad z=a^{p-k-j}b^{p}$   
So by the pumping theorem we know that $|y| \ge 1 \Rightarrow k \ge 1$ and $j+k\le p$ 
so then $w=a^{j}a^{ki}a^{p-k-j}b^{p} = a^{p+ki-k}b^{p}$  (j cancels) then, since amount of a = amount of b, $p+ki-k = p \Rightarrow ki-k = 0 \Rightarrow ki = k \Rightarrow i =1$ 
This means that any value other than 1 for i in $xy^{i}z$ means that the string is not in L therefore it is not a regular langauge.