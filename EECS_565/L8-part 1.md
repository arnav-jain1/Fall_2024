# Asymmetric Encryption 

User has a pair of keys, public and private
The user publishes public key
Knowing public key does not deduce private key


Number of keys needed for n people: $=n\frac{n-1}{2}$ 

## D-H protocol 
allows for messages over insecure media
Idea: two participants exchange intractable puzzles that can be solved easily with additional information
	Based on discrete algorithm problem
	Two users a and b pick two random numbers x and y
	a sends $g^{x}\space mod \space p$ while b sends   $g^{y}\space mod \space p$ so they both have $g^{ab}\space mod \space p$ 
Public info: g and p
p is a large prime and g is a generator of $\mathbb{Z}_{p} \space (2\le g \le p-2)$ where $\mathbb{Z}= \{1,2,3,...p-1\}$ 

Example:
	Let p = 353 and g =3
	Person A picks $x_{a}=97$ and Person B $x_{b}=233$ 
	$y_{a} = g^{a}mod\space p= 3^{97}mod \space 353 = 40$ 
	$y_{b} = g^{b}mod\space p= 3^{233}mod \space 353 = 248$  
	so the symmetric key $z_{a}=y_{b}^{x_{a}}(mod\space p) = 248^{97}mod\space 353 = 160 = z_{b}$    

How to find the generator
	For any g and p compute $g^{1}mod \space p, g^{2}mod \space p ...$ 
	For p = 11, g =10
		10 mod 11 = 10, 100mod11 = 1, 1000 mod 11 = 10
		Produces cyclic group  \{10, 1} (order 2)
	p=11, g=7
		7 mod 11 = 7, 49 mod 11 = 5 ...
		Creates cyclic group of $\mathbb{Z}_{11}$   with the generator being g=7
	Common use: p =2q+1 where q is a large prime, g creates subgroup of order q


### Why secure
Discrete logarithm problem:
	given $g^{a}$ it is hard to find a
	No efficient algorithm but not enough for DH to be considered secure
Computational Diffie-Hellman (CDH) 
	Given $g^{a}$ and $g^{b}$, it is hard to compute $g^{ab}$ (unless a and b is known)
Decisional Diffie-Hellman (DDH)
	Hard to tell difference between $g^{ab}$ and $g^{r}$ where r is random

DH is secure against passive attacks (interceptions)
Not secure against active attacks (modification)
therefore Vulnerable to man in the middle attacks
![[Pasted image 20240926204942.png]]
No way to check message is from right person

Solution 1: Select permanent public and private numbers and publish public key
Solution 2: Authenticate the communication parties and verify message is not modified