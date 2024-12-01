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
