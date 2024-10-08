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
Cryptanalysis: find plaintext from ciphertext, study weaknesses
## classic
Caesar (shift) cipher: Every char moved to the right, 25 possibilities 
Brute force or find most common letter and sub with most common letter in english alphabet (freq analysis)

Subst cipher: Swap characters, 26! possibilities 
Freq analysis go brrrr

Vigenere cipher: Each element in the key is a different shift but reciever needs the key
