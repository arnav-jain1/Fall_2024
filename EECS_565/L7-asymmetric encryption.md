# Intro
We need integrity over unsecure channels to make sure nothing was tampered with
	For integrity, we need a piece of data that verifies that the recieved message was not altered

# MAC (Message authentication code)
MAC checks to see if the message was altered in any way
MAC should be secure:
	Potentially error correcting
	*Key should be used in generation* (and therefore verification as well)
	Want a cryptographic checksum
	Confident that if verified, message was not altered
Should be small and fixed size so that it can just be appended to the original message
Collision resistant (one to one) $M(x)=M(y) \iff x=y$ 

## How to generate
### Block cipher: Cipher based MAC
Use the last 16 or 32 bits of ciphertext
Not entirely collision resistant, but good enough
![[Pasted image 20240919131859.png]]
Note: Not secure if messages are of different lengths
Also the MAC key $\neq$ encryption key 

### HMAC hash based MAC

# Hash functions
Messages no matter the size get outputed to the same hash of the fixed size 
This makes a hash function a *lossy compression*  function

MD5 broken, SHA-1 broken
SHA-2 (256, 512, 224, 384 all work)
SHA-3 (Released by NIST in 2015)
## MD 5
Original message padded always to a length congruent to 448 modulo 512 and the final 64 bits are called the Length bits (or K bits) which are a bit representation of the original message length 

The initialization buffer is loaded with {A,B,C,D} and the message is split into blocks of 512. The current buffer is saved into vars {a,b,c,d} where there are 64 operations (4 rounds of 16) performed. Each of the operations is a nonlinear function on 3 out of the 4 of the vars which the 4th var is then XORd to. The result is then rotated and saved back to {A.B.C.D} and the final value is the output
![[Pasted image 20240919133818.png]]

### HMAC
