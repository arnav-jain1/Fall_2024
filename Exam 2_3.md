# Chomsky NF

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
