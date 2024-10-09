Security concepts:
	All have 3 levels 
	Confidentiality: Items are kept secure and only the people reading can see (encryption)
	Integrity: Items are accurate and can be trusted (logs, electronic signature)
	Availibility: Items are accessible when needed (fault tolerance, backups, fair allocation of resources)

Threats vs vulnerabilities:
	Vuln is an opening or bug (weakness) that can potentially lead to an attack (crack)
	Threat is an opportunity or circumstance that can lead to the exploitation of a vuln
	Attack is the actual attack
	Control is protective measure

Vuln: 
	3 types
	Hardware: Issue with the hardware, easiest to fix. Just replace it, pull the plug, etc (undersea cables, vuln is being cut so just have backups)
	Software: Attacks to break, delete, steal make it do something different. Most vulnerable before patched (zero day)
	Data: unwanted access of data. Data is easily understandable so it is the scariest. Compromised by, wiretaps, keyloggers, bugs, asking

MOM: Attacker must have motive, oportunity, method
	Motive: Money, clout, etc.
	Opportunity: Vuln
	Method: Skills/tools
Controls: Stop attacks
	MOD: Methods of defense
		Prevent: fix vuln
		deter: Make attack harder
		Deflect: Other target more appealing to attack
		Detect: Know when attack occuring
		Recover: Mitegate attack
	Physical:
		Stop using something tangible (locks, backups, guards)
		Procedural/admin: Require/instrcut on how to react (laws, policies, ku changing pswd)
		Technical: Counter with software (OS/network access, permissions, antivirus)/hardware (firewall, smart card, uid. logging)

Principles
	Effectiveness: Controls are used properly (effecient, appropriate, easy)
	Weakest Link: Security no stronger than weakest link (humans, shitty firewall)
	Easist penetration: Attackers will attack where it is easiest, not obvious
	Adiquate protection: Protected until they lose value, protected to degree of value (spend more to protect more valuable)

# Cryptography
Cryptosystem: 
	Algorithm
	All possible: 
		plaintexts (inputs)
		keys
		ciphertexts (outputs)
	If good then:
		impossible to number keys
		find key from plaintext,ciphertext pairs decipher without key ciphertext looks truly random
Cryptanalysis: find plaintext from ciphertext, study weaknesses
Index of coincidence: Measures how similar the text freq dist is to the normal dist. For english, it should be .0686
## classic
Caesar (shift) cipher: Every char moved to the right, 25 possibilities 
Brute force or find most common letter and sub with most common letter in english alphabet (freq analysis)

Subst cipher: Swap characters, 26! possibilities 
Freq analysis go brrrr

Vigenere cipher: Each element in the key is a different shift but reciever needs the key
To crack, need to know key length and try every possible key 26^n where n is length of key
Freq analysis: Get the Index of coincidence for every string of key length n and then get average. Closest to english is likely the key length, then do freq analysis or brute force


Vernam (OTP): Key is same length as plaintext and is random XOR it and then you get ciphertext
Perfect secrecy (impossible to crack in theory) Because plaintext can be anything given a ciphertext (all plaintext equally likely) and fast but 
needs randomness, long key, unique key every time, and no guarentee of integrity

Columnar Transposition: Write plaintext as rows and the ciphertext becomes the columns. Crack via letter pairs (bigrams) and trigrams

Subst vs transposition
	Subst (S-Box, confusion) is substitute char for a different char
		Confusion: Relationship between plaintext and ciphertext is complex and confusing
	Transposition (P-Box, diffusion) is rearrange the chars
		Diffusion: Dissipate statistical structure of plaintext (many bits of input affect one bit of output) to make so small change makes a big diff

**Shannon secrecy:** 
	Guess plaintext when knowing ciphertext = guessing plaintext without knowing ciphertext. 
	Given a ciphertext, every plaintext is equally likely to generate the ciphertext 
	Perfect secrecy: iff Number of keys = plaintext and every key is equally likely

# Des
Block cipher: encrypt blocks at a time because can't tell block size and for given block of N bits, 2^N! permutations, key space is 2^K 
Des block size = 64, key length = 56
Fiestel strcuture is symmetric has L and R where L is the prev R but since we now know half the input, it is less secure so just do the operation a lot (16 rounds)
Consists of permutation, S-box, P-Box
Decryption is in reverse
Brute force will take $2^{56}$ or $2^{55}$ on average (k-1)

3des is des -> des^-1 -> des which doubles key length to 112 but slow

2des sucks because of meet in the middle, keys unknown but middle value is the same so if attacker has plaintext ciphertext pairs, then encrypt p and decrpyt c and look for matches.
	Average security is 2^56 so only 2 times better
Differential cryptanalysis traces through transformations, requires 2^47 plaintexts
Linear cryptanalysis: Approximates block cipher, requires 2^43


## Attack types
Ciphertext only
Known-plaintext
Chosen plaintext attack (CPA): attacker can obtain ciphertext for any plaintext
Chosen ciphertext attack (CCA): attacker can decrypt any ciphertext except the target

## AES
rounds/block size: 10/128, 12/192, 14/256
Each round has a nonlinear byte substitution (fixed), circular left byte shift for each row, based diffusion, and  xor the round key (derived from AES key, different for each round)
Decryption is same thing but reverse
2^127 for 128 bit key average (security strength)
Resistent to linear and differential crypt

Provable secure: ciphertext does not have enough info to determine plaintext without key (vernam)
Computational secure: Cost of breaking cipher is more than the value of the data or time to break is more than lifetime of data (block ciphers, principle of adequate protection)

## Modes
Electronic codebook Mode (ECB): One key encrypts all chunks. 
	Parallel processing, no error propogation, info leaks
Cipher block chaining mode (CBC): prev ciphertext xored with block then key applied
	No info leak, no parallel in enc, yes in dec, error prop in 2 blocks, ciphertext manip is hard
Counter mode (CTR): Have a counter that gets encrypted then xor with text
	No info leak, parallel both ways, ciphertext manip difficult (if counter doesn't repeat), no error prop
Cipher feedback (CFB): Error prop, manip hard, no info leak, not parallel enc, yes dec, error prop current and next,  (stream)
Ouptut feedback (OFB): no error prop, no info leak, manip hard, no parallel (stream)


# Asym
No verification 
MAC
	Small and fixed
	Collision resistant 

CMAC: Cipher based MAC
	Apply new key to 