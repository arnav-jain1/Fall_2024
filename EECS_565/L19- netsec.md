# Firewall
*Access control* Computer that filters traffic between protected inside network and less trustworthy outside network
	![[Pasted image 20241201151144.png]]
	There are two firewalls here, one between the internet and public facing resources (like an API) and then another between the API and private resources
	The DMZ is semitrusted and is seen as a perimeter

The firewall allows/denies traffic based on source IP, destination IP, and destination Port (as well as incoming/outgoing).
	Note that source port does not matter because it can easily be changed

May have other duties like logging, network address translation, flagging, auth

Only protect EXTERNALLY, do nothing against:
	Malicious insiders
	Connections that don't go through it
	new threats (because don't know what to block)
	not fully against viruses
Like a hollow shell


![[Pasted image 20241201152330.png]]



![[Pasted image 20241201152410.png]]
Example: Always allow TCP/UDP:... (video conf) ans send it to that specific server. ONLY allow users in that address range and if there is an unknown source, log it


Outbound firewall:
![[Pasted image 20241201153328.png]]


There are all examples of policies that determine what traffic is good and bad

![[Pasted image 20241201155531.png]]
The default policy is for when none of the policies apply
	Default deny (whitelisting), only the specified is allowed
	Default accept (blacklisting), only specified is denied

Rule order is the order that the firewall is listed, the rules cascade down so if the default is not the last one, anything after the default will never be reached
	It is first match for better performance 

Stateless: each packet considered independently (But might not have enough data)
	Filtering of TCP still possible because the first packet is SYN the others are ACK so we can filter inbound 
	We also have to make sure the replies are allowed as well 
	![[Pasted image 20241201160942.png]]
	UDP does not have connections so it is easier to spoof
	![[Pasted image 20241201161223.png]]
Stateful: Allows historical consideration (packet recorded in memory)
	ICMP can be used for reflection attacks and RPC binds to random ports which is bad which is why stateful is needed
	Stateful should be used for these protocols (makes it easier for port filtering too) and for protocols that use secondary channels for data transfer
	Each connection is stored in a dynamic state table 
		Initial TCP is logged, packets with an existing connection is checked against the table
		New packets are checked against policy and added to the table
	Stateful can handle: UDP query/response, associate ICMP with connections, solve some filtering (FTP)
	Needs to block address spoofing and deal with RPC
![[Pasted image 20241201163614.png]]

Stateless: Faster but requires more rules, less secure, and harder with multiport protocols
Stateful: simpler rules with complex policies, more secure, can do multiport (FTP), but slower


# Network address translator (NAT)
Changes the IP address when sending and recieving info
Does this by adding an element to the state table when sending and then looking up entry when inbounding
![[Pasted image 20241201164000.png]]
SImilar to packet filter because drops inbound packets if the lookup is not present but still different
Not entirely secure and does not track TCP 3 way handshake 

Circuit level gateways  (ciruit level proxy), layer 5
	Allows one network to be an extention of another by having 2 TCP connections where one sends packets to the other
	Firewall because it hides the internal network
	Kind of like ssh 
	SOCKS protocol, generic way of forwarding packets

Application-level firewalls (AKA application proxy), layer 7
	They can look at and filter based on the data and context of the app
	Less generic than circuit level gateways
	Used to detect SQL injections and others


Pros:
	Single point of control, security is centralized to one spot
	Mitigate vulnerabilities
	Transparent
COns:
	Reduced connectivity 
	Doesn't do anything for insiders

Network-layer firewalls are everywhere and DMZs allow multi leveled
Other issues:
	Perimiters of the network (where to place firewall) is getting less obvious with stuff like VPNs
	EVERY access point needs to be protected
	Hard to debug and manage
	Annoying


# Intrusion detection
Firewalls and other controls are preventitive while this is detective
Special system that monitors and detects adversaries

## Malware
Trojan horse: Program that seems legit but is actually harmful
Virus: Program that self propogates by inserting to stored code that spreads when ran
Worm: Alters already running code to spread 


## Network IDS (NIDS)
Detector in between local network and rest of internet (like on firewall or router)
Monitors selected points and examines packet header/payload
![[Pasted image 20241201172450.png]]
In this the NIDS system is deployed in 3 places
	Internet facing where it will have a very high load and high false alarms and be a glorified firewall
	DMZ sensor which protects intrusion into DMZ
	LAN sensor which monitors the intruders, insiders

Nids will look at timestamp, connection/session ID, protocol, Source/dest/ports, types, event type, decoded data, size, state related info

NIDS has a state for each connection managed via a table (similar to firewall)
	Passive and default is to let traffic pass (unlike firewall)
Analyzes in real time:
	Detects spoofing, invalid packets, port scanners, etc.
	Implements rules for violations, harmful strings, unusuals, etc.
Pros: Cheap, simple, easy to deploy and scale
Cons: 
	Inconsistent or ambigous interpretation by the NIDS system and the user make it hard to get a good rule that is effective in stopping bad and does not stop good like
		"../etc/password" might be good might be bad
		Can't also be too specific/general
		Can also mess stuff up using encodings because NIDS may use utf while the system does not
		relative paths might also become wonky and the system needs more info
	Detector and the user may have inconisistent views like
		packet expires before reaching host
		mainipulating TCP to evade NIDS 
		![[Pasted image 20241201173908.png]]
	Can't monitor encrypted traffic
Ways to fix TCP inconsistency issues
	Kill the connection, bad cause will kill good users too
	Alert when retransmission inconsistency, bad because legit retransmission is more common than illegitamite 
	Rewrite traffic to remove the ambiguities, bad cause has to operate at line-speed


## Host based IDS (HIDS)
IDS on each system that collects and analyzes data and monitors the system level events from OS logs

Strengths: 
	Works with encryption
	protects against non-network threats
	Less inconsistencies
Cons:
	EXPENSIVE
	Can be disabled
	Uses CPU/memory
![[Pasted image 20241201174738.png]]

## Honeypot
Sacrifical system that is only ever accessed by an intruder 
	False positive when legit systems access the honeypot
Example: Spam email
	Creating a secondary email for promotions so that if you start getting spams you know some company sold info
Example: Hospital records
	Creating a fake celeb record so that if staff accesses they know
Useful to detect/deflect intruders, but takes a lot of work and is difficult


# Detection
## Signature based detection
Flag any activity that is similar to known attacks
Example: TCP SYN Flood
	We know the attacker will send a bunch of packets where SYN is 1 and then no ACK
	We can calculate rate of SYNs without ACKs from an IP and if it is very high then flag them
Like a blacklist where everything is allowed except certain patterns
Can be drawn on different layers and for diff purposes
![[Pasted image 20241201180139.png]]

Signatures are characterstics of attacks that usually stay the same like
	Bodies of known worms/viruses
	return addresses of stack overflow exploits
rule needs to be up-to-date

Pros:
	SImple
	great for known attacks
	Easy to develop library of known attacks
	Can create signatures for each vulnerability 
Cons:
	New attacks will get by
	Only look at raw bytes so can do false positives 

## Anomaly based detection
Flags any activity which is different from "normal" usage
	Needs to develop a profile that defines normal usage

Generate profile by collecting a ton of data and then using stats/ml 

Pros:
	Detect unseen attacks (potentially)
Cons:
	Can fail to known attacks
	Can fail to new attacks if they seem normal
	High FP rate or high FN rate
	Needs good data

## Behavior detection
Look for evidence of the system being compromized 
Study actions leading to compromise, result of exploit, etc (not exploit itself)

Pros:
	Detect unseen attacks
	low FP
	Easy to implement
Limitations:
	Detect after it occured
	High FP if the exploit is easy to do
	Mainly for successful attacks

## Erros
False positive: System thinks intrusion but not
	Big in anomaly based attack
False negative: System thinks not intrusion but is
	Big in signature based attack (also there in anomaly but less)

False positive rate (FPR) is probability detection happened given no attack $\frac{FP}{TN+FP}$ 
	True negative rate (TNR) = 1 - FPR = $\frac{TN}{TN+FP}$ 
False negative rate (FNR) is P(no alert | attack) $\frac{FN}{TP+FN}$ 
	True positive rate (TPR) = 1 - FNR = $\frac{TP}{TP+FN}$
precision is P(alert is correct | marked correct) = $\frac{TP}{TP + FP}$  
recall is P(Marked intrusion | intrusion) = $\frac{TP}{TP + FN}$

Want FPR=FNR=0 but very very hard, instead maintain a good balance
In practice, anomaly has a lot of FP

Can also combine detection schemes: 
	Parallel composition: alert if either scheme goes off
		Inc FP, dec FN
	Series composition: Alert only if both 
		Dec FP, Inc FN

Quality of detector depends on system, sometimes FP is much more expensive or attacks are much more common than a diff system


### Base rate falacy
Accuracy is related to base rate of attacks, a very low rate could seem good but is actually not
Example: .1% FP rate
	If there are 1000 requests and 5 attacks then 1 FP a day, not bad
	If there are 1000000 req and 5 attacks then 10k FP a day, pretty bad

Occurs when assessing P(Y|X) without thinking of P(Y) and P(X)

Example: detector with 1% FP/FN
	Assume base rate of intrution at .01% (1/10k is bad)
	Let A be alarm and I be intrution 
	P(I) = .01% = .0001
	...
	P(I|A) ~ 1% which means 1% of alarms are actually intrutions

![[Pasted image 20241201184044.png]]
When attacks are rare, improving recall doesn't help much 