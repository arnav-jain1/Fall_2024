# Reference monitor
Auth system that determines whether subject can do an operation on the object
	Monitors all data access to enforce security policies

Complete mediation: Montiors ALL operations that are security sensitive 
Tameperproof: Protected from unauthorized process
Verifable: Simple enough to be tested and proven correct

They take requests as input and then output true or false whether they have access to it or not 
enforces *mandatory* protection system which means no flow from high classification to low classification
	![[Pasted image 20241025112229.png]]
	In this case, Bob can r/w from Data file so it denied ability to write to back-pocket file in case bob tries to steal info and export to a lower perm file

## Mandatory Access Control (MAC)
Subjects and objects represented by labels
	Set of labels are immutable (unchangeable) except for trusted admins and trusted programs
	![[Pasted image 20241025112622.png]]
	This shows that if you have secret classification, you can read and write to secret and only read from unclassified and trusted. 
	If you are trusted, then you can write to unclassified (but not read), r/w from trusted, and write to untrusted

## Trusted OS 
Trusted OS is a trusted program: It contains
	Functional correctness, enforcement of integrity, limited privilege, and appropriate confidence level
Kernelized design
	OS kernel handles low-level hardware resources
	The security kernel is even smaller than the kernel and handles security
	Reference monitor is part of the securitu kernel
![[Pasted image 20241025113042.png]]
It needs to have:
	Policies: what security rules that are enforced
	Measures and mechanisms: how to enforce the policies
	Independent eval
Trusted computing base (TCB) is the neccessary part of the OS that affects security
	Changes in TCB will affect security
	Changes in not TCB will not affect security
	![[Pasted image 20241025113323.png]]
	Includes certain files, hardware, processes, memory, etc
Security features (functions involving security):
	Sharing enforced
	IPC and sync
	Protecting OS data
	fair service
	**Memory protection**
	More

# Memory management
Important!! 
	IO done by accessing memory 
	All data/operations located in memory
	IF leaked, shares sensitive info

Hardware mechanisms
	Fence, registers, tagged architectures, segmentation, paging

## Fence
Boundry between OS space and user space, users confined to one side
Fence register used to keep track, keeping it static is bad
	Contains address of end of OS space
![[Pasted image 20241025114019.png]]
Protects OS from user but not user from other users

## Base/bound registers:
Programs are written like they start at 0 and offset to any address
Base register is the lowest (0) and bound register is the highest (moves)
Protects users from each other but is all or nothing

Tagged arch:
	Takes the base/bound registers and extends it for more fine grained access control
	2 pairs of base and bound registers, one for the code range and one for the data
	You can mark every byte as read, read/write, or exec. 
	Set by priv OS but only a few arch use this model because lots of overhead


## Virtual memory
Segmentation divides program into pieces and relocates them however the OS wants
	Good for perf
	Each segment has diff access rights
	each has a unique name and size
	![[Pasted image 20241025114924.png]]
		denoted: < segment name, offset >
Program doesnt need the true memory address to use the segment
	The os can move and assign a segment to any address and can also remove it
	OS can check EVERY address reference when used
For security, it ensures the offset is not beyond the end of the segment (seg fault)
![[Pasted image 20241025115326.png]]


Paging is another way to do the same thing except pages are of equal size pieces (512-4096 bytes)
	Referenced by number instead of name
![[Pasted image 20241025115543.png]]
![[Pasted image 20241025115638.png]]
You can also combine the 2


## Virtual machine for secuirty
Sandbox: protected env that stops other programs from endangering the env
Honeypot: fake env to lure attacker to learn goals, techniques, tools, etc


# Memory attacks
Memory has a lot of important info and hard to encrypt (slow) so 2 types of attacks:
	Hardware
	Software
Software:
	Bugs in the system allowing people to read addresses
	swap and dump:
		Memory contents are sometimes stored on harddrive when you get core dumps, swp files, crash reports
		Cause one of these conditions and examine the dump file
		Can contain passwords/emails
	Find and read uncleared buffers

Physical:
	Bypass OS/CPU and directly read the ram
	Cold boot attacks:
		Contents of ram fade slowly after power is turned off, the colder it is the longer it takes
		Remove the RAM after power off (but keep it cold) and then read it from another computer, bypasses a lot of stuff
	Or direct memory access

## Rootkits
Type of malware that becomes the root
Intercepts and changes OS processes by becoming part of kernel
Can hide malware (like itself) and persist for as long as possible



