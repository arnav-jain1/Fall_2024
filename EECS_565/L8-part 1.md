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

# Public key cryptography (Asym enc)
Each user has their own pair of keys (public, private)
Key owner lets public key be public (private hard to infer)

## Requirements:
Computationally easy to encipher/decipher
Computationally infeasible to determine private key from public
Computationally infeasible to get private key from chosen plaintext attack

## Components
Easy to make public/private key $(pk, sk)$
Encryption: Easy to encrypt given a public key $C=E(pk, M)$
Decryption: Easy to decrypt given ciphertext and priv key


# RSA
Based off difficulty of factoring large composite numbers
e = public key
d = priv key
Recommended is 2048 bits
Encryption:
$$c = m^{e} \space mod \space n$$
Decryption:
$$m = c^{d} \space mod \space n$$
Signing is the same thing
**Note:** Exponentiation is MUCH slower than AES


## Key gen
1. Find 2 large primes p and q (p =/= q)
2. n = p * q  and $\phi(n) = (p-1)(q-1)$  
3. Chose a random int e so that phi and e are relatively prime: gcd(e, $\phi(n)$)=1 public key is (e, n)
4. d = $e^{-1}mod \phi(n)$ and then (d,n) becomes the private key
This is then used in encryption and decryption
This has confidentiality, integrity, and auth

## Why good?
Do NOT use same n among multiple users
Factoring problem: Prime factorization is hard and takes a while
Given n = pq, c and e, find m s.t $m^{e}=c \space mod \space n$ 
	finding eth root of c mod n, solvable if n is factorizable (which it is because n = pq)
	No known effecient algo, need p, q
Cant find d given (e,n)
	As difficult as factoring n

## PRactice
Message m should be less than n (use small plain text)
	Usually used to encrypt a symmetric key
RSA output is determinisitc, attacker can easily try all possible plaintext
Solution:
	use larger keys: 2048
	Use padding called PKCS#1 OAEP
	Instead of encrypting message M, we encrpyt: $M \bigoplus H(r)$  or $r \bigoplus G(M \bigoplus H(r))$ where r is random and G/H are hash functions (sha256)
	This verifies integrity of M

Primary use is to transfer secret key
Can also be used for key agreement: 2 or more parties make a secret key by exchanging information according to a protocol 
	Used when 2 identities unknown
Build Public Key Infrastructure (PKI)
	Bind public key of user to digital certificate used by the user
	Certificate verfied by digital sig

Integrity and auth
Generate digital signature
	User can sign a message
	other users can see the public key and verify the signature

# Dig sig
compute a sig: sign with private key because Only the sender can access it
Verify with public key which everyone can do

Let there be a key (public, secret)
sign it: $\sigma = sign_{sk}(m)$ 
Verify it: $verify_{pk}(m,\sigma)$, if valid, accept

Textbook RSA (insecure)
d = private, e = public
Sign $\sigma = m^{d}mod\space n$ 
Verify $m=\sigma^{e}mod\space n$ 

Sig vs MAC
Digital signature generated by private key using public key algo
	Private key is unqiue to the user so only the user can generate the key
MAC is generated with a secret key using secret key cryptographic algorithm or a keyed hash scheme
	Secret key is shared so the reciever can also generate the same MAC

