# Password based Authentication
Process of verifying identity of user
Done using credentials which can be
	Something one knows: Password, passphrase, a secret
	Something one has: IP address, certificates, devices
	Something one is: biometrics


When logging in, username is identification (who someone is) and password is the authentication (how do we know that person is actually that person)

Can be bypassed using stealing or exploits
	Steal: Sniffer/keylogger to steal passwords (or just like watch them)
	Exploit: Get password hashes and crack

Store the hash(password) with the userID 
	Hashing is determinisitic so it always makes the same hash
	Onewayness: Hard to deduce the password form the hash
	Slow to compute: the slower the compute time the longer it takes to brute force

Linux login:
![[Pasted image 20241011112450.png]]
You add a salt (random chars to the front) to make weak passwords easier to crack
## Brute force
Brute force attacks involve having a file full of hashed passwords and hashing passwords to see if there are any matches

Password strength depends on length. Easier to brute force all passwords of length 6 than length 16
Sum the possible chars then exponent the pass length

## Dictionary attacks
Precompute hashes of common passwords and see if there are any matches in password files. (know immediately)
Rainbow tables are similar
You trade space for speed

## Countermeasure salting
Unique random value (not a secret though) chosen for each user so the new password hash is sha-256(salt || password)
Now same passwords have different entries and more randomness making dictionary attacks and rainbow tables worse
When using dictionaries, the user needs to try every possible salt for every possible password, 12 bit salt has 2^12 hash values


## Online attacks
All the previous attacks are offline, can be done without access to the service
Online attacks interact with the service 
	Ex: Try every password for a username
	Stop by:
		Limiting attempts
		Captchas, find when people are using scripts

## Other risks
Weak passwords
default passwords (using admin and 1234 for login)
Broken implementation 
Key loggers
Social engineering


## Improvements
Improve password strength by increasing length, special chars, and numbers
Password managers can help but what if they are compromized 
Graphical passwords are also being implemented
Biometrics are best because no need to remember and hard to fake
Multi-factor auth: 2 factor

# Kereberos
Single sign on, sign in once and you get access everywhere
Most used centralized password system

## Distributed auth
Threads:
	User Impersonation: Malicious user with access to workstation pretends to be someone else
	Network impersonation: Malicious user changes network address to impersonate another workstation 
	Eavesdropping, message modification, and replay attacks

Proving user identity when requesting multiples services needs many to many auth
	m clients, n auths
	Every server knowing every auth would be too much 
	If attacker breaks one, they know all
Solution is Kerebros
	Secure against passive eavesdroppers and active malicious attackers
	Transparent: users only enter password but they don't know what is going on
	Scalable: can serve many users/servers

Key idea: Use a third party, authenticate with Kerebros which then issues a ticket, the ticket requests access to the computer.
	Server has no auth
	Client has only one pass
	*single point of failure*

It is also insecure to send passwords/ticket in plaintext and no need to authenticate every time using a service
To fix the security, convert the password into client master key, Ka and share Ka
Then share an encrypted ticket/session key
![[Pasted image 20241011115114.png]]
Ticket needs to have:
	Username for impersonation
	Server name
	Address of user's workstation for network impersonation
	Session key
	Ticket lifetime, 
	etc
Since it is encrypted, no eavesdropping, message modification, etc.

If user wants to access multiple tickets have 2-step auth where KDC gives a TGT which is then used to get a service ticket from TGS
![[Pasted image 20241011115547.png]]
Remaining threats:
	Intercepting service ticket and Using same workstation
	Crack password and impersonate Alice

To stop the first one, show proof via authenticator along with the certificate and have the ticket expire after some time
Servers must also show proof to the user
![[Pasted image 20241011120122.png]]

## Overview
Long term sym keys: Used to get short term keys
	Ka of client known to user and KDC
	Ktgs of server known to TGS and KDC
	Kb of network server known to the server and KDC
Short term sym keys: Unique key for each client server pair (session key)
	Ka-tgs: session key with client and TGS, made by KDC
	Ka-b session key between client and network server, made by KDC
	Stored in memory because short term

Centralized auth service with mutual auth
Based on symmetric crypt
Less keys for clients
Less work for servers, slightly more for users:
	KDC has long term keys, not servers
	Client has short term creds and manages them
Less communication overhead
more scalable




