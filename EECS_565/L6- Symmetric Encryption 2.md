# More cryptanalysis of DES
Assume attacker knows plaintext and ciphertext
## Differential Cryptanalysis
Technique for tracing what each round of transformations does
Compare how changes in the plaintext change the ciphertext and this gets a statistical distrbution of the key or key bits
needs $2^{47}$ plain texts to get key

## Linear Cryptanalysis
Simplify the block cipher with XORs
$2^{43}$ pairs needed


## Terminology
Ciphertext only attack: only know ciphertext
	Freq analysis
Known plaintext attack: Know some plaintext, ciphertext pairs
	Man in the middle
Chosen plaintext attack (CPA): Attacker can get the ciphertext for any given plaintext
Chosen ciphertext attack (CCA): Attacker can decrypt any ciphertext except given ciphertext

Provable secure: if the plaintext cannot be inferred from the properties of the ciphertext, it is unbreakable
	One time pad
Computational secure: if the encrypted data is not worth the money needed to crack it OR if the time it takes to brute force the ciphertext is more than how long the data is useful, then computationally secure

## Weakpoints
56 bits is too short of a key, cracked in 22 hours in 1999
3DES is too slow with small blocks


# Rijnadael algorithm (AES)
Symmetric key
Block size is 128 bits = 16 bytes
128/192/256 bit keys
Stronger and faster than 3DES
	All bits operated instead of halves so only 2 rounds needed for full diffusion
	10/12/14 rounds for 128/192/256 bit keys (much better than the 48 for 3DES)

Each round has 4 simple operations
1. Nonlinear sub
2. Circularshift of bytes in each row (0->1, 1->2, 2->3, 3->0)
3. Mix columns (diffusion)
4. Add round key
![[Pasted image 20240916222113.png]]

AES has a state array of 4 byte cols
	Fill in each column (down column first) then use the value for substitution on input bytes
	If input % 16 $\neq$  0, pad the input
![[Pasted image 20240916222436.png]]

1. Byte substitution
	Use Rijndael S-box to convert input byte into output
	Non-linear, based off of polynomial arithmitic
	![[Pasted image 20240916222605.png]]
	S-box is a 16x16 table so if the input byte $S_{0} = 01110101$ then split in half so 0111 0101 which is 7 and 5 respectively. 7,5 in Sbox is = 9D which is 10011101
2. Row based diffusion
	Shift each byte in each row to the left cyclically (so 1st row = nothing, 2nd row = 1 byte to the left, 3rd row = 2 byte to the left etc)
3.  Column based diffusion:
	Each byte in each column mixxed up in some way
	Each byte in the column is given its own operation (which can vary) which is then XORed all together
![[Pasted image 20240916223332.png]]
4. XOR the round key with the current state
	round key is 128 bits and is derived from the AES key
	Processed by column
![[Pasted image 20240916223446.png]]

### Key expansion
To get a different key for each round, we need to use key expansion 
Per round, key needs to be 128 bits= 16 bytes = *4 words* (col of 4 bytes)
so 128\*n + 128 = num of bits in a key (n = num rounds)
key expansion is done to each word
![[Pasted image 20240916223825.png]]



## Complete round
![[Pasted image 20240916223850.png]]
State subbed to a new state where the rows are all shifted then columns are mixed where they are XORed with the round key

## Decryption
Run cipher text forward direction but with inverse operations
	XOR itself
	inverse the shifts
	inverse the mixcol table
	Use round key in reverse

## Cryptanalysis
Effecient and secure
$2^{127}$ stregnth
No successful attacks *so far*
	Resistant to linear cryptanalysis which relates the input to output but since a polynomial is used instead it cant
	Resistant to differential cryptanalysis 
	New attacks (timing attacks and side channel) based on implementaion

Only computational guarentee 
	Not impossible just expensive and time consuming
	Only brute forcable if there is no algorithm for it
Principal of adequate protection: The value of the decrypted message is less than the time/money it takes to crack it

# Block ciphers
How to encrypt large plaintext:
	Divide into blocks, pad the last block, then encrypt

If you split the text into chunks and use the same key for each chunk, then 
	if the plaintext chunks are the same, the ciphertext will be the same which is an *information leakage*
	If the ciphertext is intercepted and the order of the chunks are modified, then the information decrypted will be different and it will be hard to tell that anything has change which is *ciphertext manipulation*

### Mode of operations
Ways to generate plaintext chunks and keys

Electronic codebook mode: Each key for each chunk
	Not recommended to use for long plain texts
	Pros:
		Blocks can be processed at the same time (parallel processing)
		If there is an error in one block, it won't affect the rest
Cipher block chaining mode: Ciphertext of prev block XOR plaintext of curr block
	1st input XORed with initialization vector
	![[Pasted image 20240916225954.png]]
	traits:
		No information leakage 
		No ciphertext manipulation
		Parallel processing in decryption only
		Error propagation 
Counter mode:
	Have a "counter" (chunk of incrementable text that is of same size as plaintext) and then encrpyt it. Then XOR it with plaintext
	Traits:
		No info leak
		harder to manipulate 
		Parallel processing both ways
		No propagation
Cipher feedback mode:
	The previous ciphertext is encrypted and then used to XOR the new plain text
	Block manipulation is harder, Parallel processing in decryption, error propagates 
Output feedback mode:
	The previous subkey is 
<mark style="background: #FF5582A6;">	Ask how this is different</mark>


# Symmetric encryption algos
2 primatives:
1. Block cipher, encrypt long data blocks with same key
	Used in blocks of data (files, emails, db)
	Advantages:
		High diffusion 
<mark style="background: #FF5582A6;">		more immunity to insertion</mark>
	Disadvantages: 
		Slower, error propagates 
2. Stream cipher: encrypt short data blocks with changing key
	Usually one symbol at a time with a simple operation like XOR
	Used in streams like communication
	Ad:
		Fast, less code
		No propagation
	Disad:
		Low diffusion
		vulnerable to modifications
	