# Network security controls
Controls refer to mechanisms or safeguards (either technical or administrative) designed to minimize security risk and ensure confidentiality, integrity, and availability of the network


## Design/architecture
Segmentation/seperation: Different computers on different networks entirely 
![[Pasted image 20241126125009.png]]
Like partitioning


DMZ:
![[Pasted image 20241126130007.png]]
An area between the server-side firewall and the internal firewall. This allows better speed and more safety


### Encryption
Most important and best control against many threats but not always enough

Link encryption: Messages decrypted at routers, transparent to the users (upper layers)
	Protects data over untrusted physical links
	![[Pasted image 20241126130316.png]]
End-to-end encryption: Data decrypted by the application
	No decryption in transit
	Protects data against lower layers
	![[Pasted image 20241126130355.png]]

Not infalliable as stuff can happen before/after encryption/decryption (like trojan horse), does not protect against other vulnerabilities like DoS.
	Also dependent on good key management

## NetSec protocols
### SSL/TLS
Secure sockets layer or Transport layer security
Designed for ecommerce, protect against eavesdroppers and MITM attacks

Authenticity: Server and client (optional) have to authenticate
Confidentiality: Encrypted communication with socket interface (any socket can run on top of SSL)
Integrity: Prevents replay attacks

Protocol:
![[Pasted image 20241126131843.png]]
1. Negotiate parameters: Both client and server generate random 256 bit num (Rc and Rs)
	Also includes deciding cipher suite, protocol versions, etc. (choice of keys)
2. Authentication: Client verifies the server certificate (knows sever's Pk)
	Implicit and anonymous authentication (RSA)
3. Key exchange: Client generates and sends key (encrypted by Pk), client cert is now sent and verified (optional)
	Encryption during key exchange done using DH or RSA
4. Session: messages are exchanged

Forward scecrecy: Recording a connection and then obtaining secrets later does not compromise the recorded values
	RSA: For example, with RSA, if the Keys (Rc, Rs, and PS) are recorded and the private key is stolen, then the info can be derived
	DH: Guarenteed forward secrecy because the PS is deleted after the session is over so even if the opp gets the secret key they can't get the info. The DH component is also signed so opp cant MITM without the server's private key


TLS 1.3 only does DHE and stronger crypto algorithms like AEAD 
	AEAD is block cipher that ensures conf and integrity
Also only 1 round trip handshake
![[Pasted image 20241126133630.png]]
![[Pasted image 20241126133700.png]]
TLS 1.2:
	Client sends random info + potential crypto params (like algorithm, key length etc), server sends same stuff back (but actual ones)
	Client generates PS then encryptes and sends
	Server then sends key back and then ready
TLS 1.3
	Client sends random info + params and then also generates part of DH (KeyEx part)
	Server does the same thing
	Now they both have their half of DH so they have a mutual secret (no direct exchange) and are ready

### SSH
Secure shell protocol that has confidentiality, integrity, and authentication

Flexible authentication (can use pswd, secureID, kereberos)
Similar to SSL: Has clients, servers, and sockets
	No certificates, PK of host known
Can't prevent TCP/IP attacks, cracking, or traffic analysis


### IPSec
Part of IPv6 (backported to IPv4)
Two options:
	authentication (AH)
	encapsulated security (ESP)
Two modes: 
	Transport
	Tunnel

Constructs a secure channel on the IP layer (sits between IP and TCP/UDP layers)
	Transparent meaning apps don't need to be changed to use it
	Requires modifications to protocol stack or kernel level
Security features:
	Integrity: Ensures data wasn't messed with
	Authentication: Verifies identity of communicating parties
	Confidentiality: Data private through encryption
	Replay protection: Attackers cant capture and resend packets
	DoS protection

![[Pasted image 20241126140315.png]]

Flow:
	Data goes through transport layer (UDP/TCP) and hits the IPSec layer where it is applied
	Data then flows through IP as normal then hits the layer again and then handled as normal again
Specified in policy management, packet processing, and key management


Transport mode: (Host IPsec aware): end systems initiate and are recipient of traffic. 
	Encrypt payload but not headers
Tunnel mode: gateways act on behalf of hosts to protect traffic
	Payload and headers enc
![[Pasted image 20241126140748.png]]

Authentication Header: 
	Add header with auth data
	Integrity
ESP: 
	Encapsulate the datagram rather than adding a header
	encrypt and authenticate
	conf and integrity
![[Pasted image 20241126141718.png]]
