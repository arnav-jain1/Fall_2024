Goals
	Confidentiality: Only sender and reciever can see
	Integrity: Ensure not altered
	End-pt auth: Sender and reciever can confirm identity

Cryptography: conceal data against unauthorized viewers
Encryption: Process of encoding a message so the meaning is hard to figure out
Decryption: Process of decoding an encrypted message

# Cryptosystem
system for encrypt and decrypt
Includes:
	Algo
	All possible input (chars, symbols, bits)
	All possible outputs
	all possible keys

Need to know
1. How plain text becomes cipher text
2. How plain text is processed and turned into encrypted message
3. How key is made

Cryptanalysis: Study of decryption without knowing secrets (cracking). Uses weakness of design
	Know algo and ciphertext
Cryptology: Cryptography + Cryptanalysis
## Cesar cipher
Every char shifted (type of substitution)
Old
Shift cipher with K=3
Shift cipher: 
$$\text{encryption:} E_{k}(m) = (m+K)mod26$$
$$\text{Decryption:} D_{k}(c) = (c-K)mod26$$
Only 25 possibilities 
How to crack: Frequency analysis
	Find most common letter, maps to E
	Then we know the shift so decrypt
	Or brute force
	Most Common letters
	![[Pasted image 20240906120223.png]]
	Also can use bigrams and trigrams (consec 2/3 letter sequences and use that)

## Subst cipher
Substitute one letter with another 
Monoalphabetic: each letter only used once
Key is the mapping from one set of 26 letters to another

$26! \approx 4 * 10^26$
How to crack:
	Brute force doesn't really work
	Freq analysis, most common letter is e, second is t, 3rd is a
	Then start predicting common words like the, is, I, a, etc.
Improvements:
	polyalphabetic: use multiple alphabets
	Homophonic: Multiple letters for one input
	Polygram cipher: encipher groups of letters at once

## Vigenere 
Creates a table (Vigenere table)
	Each row in table is a different shape (shift over sub because easier to communicate)
	Sender and receiver need to know and agree on sequence of rows (key)
So the possible shifts are 0-25 so if the key is {5, 8, 1, 19, 25, 11} then the first letter is shifted 5, second shifted 8, etc and this loops. 
	Note this assumes the table is sequential, the order can be mixed up too! (ie row 0 is shift 2, 1 is shift 7, etc) The table must be consistent (or just use an equation)


Pretty hard to break:
	Guess key length
	For each guessed key length, freq analysis 
	Dictionary attack: If possible keys are known then try every possible key
	
