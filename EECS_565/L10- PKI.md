# Auth public keys
Scenario: 
	Alice has a message from Bob, message m was hashed into h(m) and then encrypted 
	Alice needs to check if this is correct and it matches but Alice needs Bob's public key
	How can alice confirm the public key was Bob's instead of someone else's
Solution: 
	Sign Bob's public key but with who? 
	Need a trust anchor, something we can implicitly trust 
	Signed statement specifying identity and PK
		<Bob, PKBob, sigsk(Bob, PKBob)>
		But who is sk?
## Attempt 1: Trusted directory (TD)
Public dictonary of users and their public keys and everyone goes to it to get the info
Ask directory for public key and TD will create the certificate
TD needs to verify who they are sending the key to 
Also is not scalable and has one singular point of failure

## Attempt 2: Public Key infrastrucure (PKI)
Designed to fix the scalability issue by introducing hierarchical trust
	The root of the trust is known and trusted by everyone
	It delegates trust to other authorities which can then sign and give users within those authorities the same trust (delegation is signed too)
		For example, root signs alice and bob's pk
		Now Alice and bob are trusted who can then sign other people's public key
	This way the root doesn't have to sign everyone
If root (or Alice/Bob) become comprimised, everyone stemming from that root is comprimised 

Goal of auth: Bind identity to card/token/pswd/key/cert
Goal of PKI: Bind identity to public key
	Needed to communicate with other people
	Erroneous binding = No secrets between users
	Assume user is identified by acceptable name (common name)

PKI includes:
	Certificate authority
	certificates
	Repository to get certificates
	Method of getting the chain of certificates from known public keys to target
	Method of removing certificates

It is like an ID card. When you go to the DMV, you trust Kansas, KS is the trust anchor

### Certificate authorities (CA)
CA is a trusted third party that issues certificates
	Can also be a CA for ourselves (self-signed certificates)
	

### Trust models
Hierarchical CAs with cross-certification (Multiple roots that certify each other)
Oligarchy model: 
	Browsers and OSes come pre-configed with root CA's certificates
	New ones can be added/removed
Distributed model:
	No root, users certify each other for "web of trust"

Trusted root authority helps make a certificate chain. User 1 certifies user 2 which certifies user 3...