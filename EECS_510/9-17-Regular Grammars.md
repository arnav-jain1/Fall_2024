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
