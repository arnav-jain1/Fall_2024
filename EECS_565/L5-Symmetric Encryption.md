# Recap
Shannon's theory: Perfect secrecy $\iff$ possible keys == possible plaintexts && P($key_{1}$ == P($key_{2})$ ( every key equally likely)

2 operations 
1. Substitution (S-Box) for confusion 
2. Permutation (P-Box) for diffusion

# DES
Federal standard used for over 26 years
Crypto algorithm with:
	High level of security
	Effecient
	Open
	Useful in many applications

### Block cipher
Split input into blocks because it is hard to figure out block length 
For block of n, there are $2^{n}$ permutations
Vigenere is not a block cipher


DES is a Block Cipher
	Block size = 64
	Key length = 56 + 8 parity bits
Each block is a Feistel structure
	Input 64 bits: Split into 2 parts
	Output: 64 bits 
![[Pasted image 20240913111043.png]]


This is symmetric, you can use the same structure for encrypt and decrypt
Not secure!
	$L_{i}=R_{i-1}$ means we know half the input
	Solution, do the same operation 16 times

F function
	Called mangler function
	Permutation with expansion 
	S-Box and P-Box
![[Pasted image 20240913111312.png]]


PermutatioN:
	16 bits are used twice
		These bits of plaintext affect more of ciphertext
		Expand $R_{i}$ to 48 bits, same as the subkey
	XOR with subkey ("key mixing")

S-Box:
	Shrint $R_{i}$ from 48 bits to 32 bits 
	DES uses 8 predesigned S-boxes that are all unique substitution
	Input: 48 bits divided into 8 blocks
	Output: 32 bits, 4 bits from each block
	Each S box goes from input of 6 to output of 4
	Bits 0 and 5 define row, bits 1-5 define col
![[Pasted image 20240913111920.png]]

P-Box
	Rearrange bits following a fixed function to add confusion
	This also ensures that in the next round, the output bit can go to a different sbox next round
	Makes it so 1 change in plaintext makes a huge difference
![[Pasted image 20240913113321.png]]


Key:
	Key is 56 bits with 8 bits of parity
	Each round we need a subkey that is 48 bits
	Steps:
		Divide 56 bits into 2 halves, 
		Circular shift left

##  Encryption Summary
Block size = 64 bits
Key = 56 bits
rounds = 16


## Decryption
Same structure as encryption
Use the round keys in reverse order

# Cryptanalysis of DES
Brute force:
	Need to try all possible keys (56 bits so $2^{56}$)

## Key exhaustive search (KES)
Attacker intercepts q plaintext cipher pairs encrypted with the same key K
Attacker tries all possible keys that are k bits long that satisifies $E(K_{i}, M_{i}) = C_{i}$ 
How many tries?
	$P(key = K_{j}) = 1/2^{k}$ k = number of bits
	Average number of attempts $2^{k-1}, k = 56 \Rightarrow 2^{56}$  
In 1999 it was possible to do it in <24hrs with distributed machines


## Triple DES
Encrypt plaintext 3 times with 3 different keys
	DES + DES^-1 + DES
	Uses 2 or 3 keys (Key 1, then 2, then 1/3)
Increases key length to 112 
	Weakness is that its slow

## Double DES
Encrypt plaintext twice with 2 different keys
Weak af tho
K1 and K2 are unknown but the same value is used in encryption and decryption
Can run 2 parallel exhaustive seaches
 1. Attacker gets plaintext-ciphertext pairs
 2. Guess all possible K1 (2^56)
 3. Encrypt P1 with all possible K1 and create a table with all middle values
 4. Decrypt C1 with all possible K2 and create a table with middle values
 5. If there is 1 collision (same val), in the middle values, thats the corresponding key, if >1, repeat for P2,C2
Needs 2 parallel searches of same size so $2 * 2^{k+1}=2^{k}=2^{56}$, double the time, double the safety