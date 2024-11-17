Passive attacks: Getting info like Sniffing (reading message content), traffic analysis 
Active attacks: Doing damage like DoS, spoofing, modifcation of message, deletion

Standards:
	Key dist and auth: PKI, Kerberos
	IP Security: IPSec
	Transport layer security: SSL, TLS
	Application layer security: HTTPS, SSH
	Email Security: PEM S/MIME, PGP

Firewalls: Prevent intruders from getting into system
Intrusion detection systems: If firewalls fail, detects intruders
Response: Recovery and forensics 
Worms, botnets, and more

# Network vuln

Attacks can be launched from 1000s of miles, can also hack a machine from a different machine
Many points of attack
Network complexity
Unknown Paths (packet may travel over many different paths until it reaches a dest)
Unknown permiter (Being on multiple networks)
	![[Pasted image 20241116202934.png]]


# Network attacks
Prior to an attack, scouting is done via things like port scans, dumpster diving, network mapping, etc.
	Packet sniffers to see packet info
	Cable based wiretaps to read signals
	Can also get info from insecure wireless networks

Packet sniffing is a way to read packets and then extract their info
	![[Pasted image 20241116203208.png]]
	Can be done because many protocols are not done with security in mind

Can also spoof packets because source is really easy to modify 
	Done at link layer with ARP
	Network layer with IP packet
	or transport layer with TCP/UDP packet

Address Resolution protocol (ARP) spoofing
	Translate layer 3 IP addresses to layer 2 MAC addresses
	![[Pasted image 20241116203525.png]]
	For example, Alice wants Bob's IP MAC address who is on 1.2.3.4 so she asks 1.2.3.4 but then someone on the same address can respond with theirs 

Network layer spoofing (ICMP redirect)
	ICMP redirect message tells the host of a different route and redirects
	Attacker sends a spoofed ICMP with a malicious redirect url 
	Like a MIM to get info


Spoofing at transport layer:
	TCP makes a 3 way handshake
	![[Pasted image 20241116204212.png]]
	ISN is a 32 bit sequence number assigned to a new connection, used allong with acknoledgement number (ACK) to auth a remote entity
	Non-Blind spoofing
		Attacker can impersonate the client by sending his own ISN (only involves being on same WiFi because can sniff)
		![[Pasted image 20241116204526.png]]
	Blind spoofing (not on same network):
		Attacker tries to predict ISNs (sequence number prediction)
		More sophisticated and harder

After spoofing you can be a MIM, hijack a session (by reestablishing a connection), or even DoS by covering your tracks 


## DoS/DDoS
Attack the resources of a server (like CPU, RAM, etc) to prevent the server from working properly

### Ping of death
Send an oversized ICMP packet (more than TCP max of 65,536 or ICMP echo max of 64)
After recieving oversized packets, buffer overflow occurs potentially causing crash, freeze, reboot

### Source address spoofing
Attacker uses forged source addresses to generate a ton of packets with the target victim as the destination
Prevention is hard, but can use honeynet which is like a honey pot where there is a fake IP and the packets are sent there


### TCP SYN spoofing
Don't send back the ACK packet (3rd one) leaving the connection on the server open in its local table
Causes future legit requests to be rejected

To mitigate: SYN Cookies, Server sends SYN/ACK with a cookie 
	f(src/dst IP/port, timestamp)
	Doesnt store requests in queue then


### Flooding
Send packets (like ping) as fast as possible to overwhelm target
Can also be done with UDP or TCP SYN or any other packet 
Source is easy to find unless spoofed
*Attacker needs to be on higher-capacity link*

Stop by filtering packets or detect the spoofing


### Reflection attacks
Attacker spoofs the victim's address to send DNS queries using port 7 creating a loop
	Port 7 is for echo so they just echo each other
	![[Pasted image 20241116210859.png]]
Attacker sends a request packet with the victim's IP as source to an intermediate server which will generate response packets to the victim
	Intermediary usually a beefier PC to make it worse
Can use any TCP/UDP service

Scale up to ddos
Countermeasure: Ignore all spoofed source packets

### Amplification attacks
Like reflection, but attacker sends to multiple intermediaries that just hammer the victim
Smurf
	Attacker spoofs IP to victim
	Sends a ping to a broadcast address (address that broadcasts to other addresses)
	Every host replies flooding the victim
	![[Pasted image 20241116211325.png]]
One req = multiple responses


DNS amplification:
	Legit DNS used as intermediary 
	Spoofs source then sends request to DNS
	In DNS protocol, a 60-byte req becomes 512 (IPv4) or 4000 (IPv6) byte response

Small request -> Large response

Overall:
	Increase number of responses to target: Smurf
	Increase size of the packet: DNS
	Increase number of requests directly: DDoS

### DDoS
Get access to vuln systems (exploits or trick via trojan)
Creates a zombie network to attack victim
Overwhelms the target

Combined with DNS to make DNS Amplification DDOS
	Open recursive DNS attacked from botnet resulting in 1tbps 

### DNS Water torture
How Recursive queries work
![[Pasted image 20241117121757.png]]

DNS water torture is sending random DNS queries with not found domains
The queries go all the way to the authoritative server
![[Pasted image 20241117121953.png]]


## DDoS pervention
Hard to do because hard to tell whether traffic volume was legit or not
Defenses:
	Attack prevention: Don't let the attack happen in the first place
		Block spoofed source addresses, IPs, servers
		Use reverse filters
		Manage attacks using CAPTCHA. rate limit, etc. Also use syn cookies and such
		Overprovision: Get enough bandwidth that it is hard to overwhelm
	Attack detection and filtering: During the attack, filter the packets
		Capture and analyze packets to figure out attacks and then prevent (signature or abnormal patterns)
	Source traceback and identification: During and after, try to find the source
	Attack reaction: response after attack
		Have the ISP trace the packet back to the source (hard and time consuming but needed for legal action)
		Contigency/response plan
