# Software security
## Bugs, Vuln, Exploits
Bugs: Programs that don't work as intended
Vulnerabilities: Malfunction that can be used by malicious users to exploit the program
Exploits: Set of operations to use a vulnerability


Intelligent adversaries force bugs into the worse possible conidition to perform their attack
Software is secure when it can perform correctly while being attacked

Approaches to find vulnerabilities:
	Code review: Inspect code manually
	Static analysis: Automatic code analysis with programs
	Dynamic testing: Running the code with various inputs to ensure function

## Types
Memory corruption
**Buffer overflow**: 
	Normal conditions: Causes program to fail
	Adversarial conditions: makes the security of the system compromized 
	Very common, very simple, been around for decades

### Buffer overflow
A buffer is a finite capacity area for data storage that is *pre-defined*
	Like uninitalized array

Buffer overflow is when more info is written than what can be held causing info to be overwritten
Buffer overflow attack is when the code is rewritten into new code that can crash/compromize/etc the system
![[Pasted image 20241116130010.png]]
Happens when the input is more than the boundry and the program doesn't check

![[Pasted image 20241116130851.png]]
So now if that is written to B and B stores a variable used for auth purposes, then it could be really bad

And B can be written to:
	Another piece of the same program (relatively safe)
	Code of another program (bad)
	data/code of OS (really bad)


## Stack overflow
### Short recap:
![[Pasted image 20241116131110.png]]

Stack pointer points to the top of the stack and is relative to the base pointer (stored in %ESP)
Stack frame is the space alloted to each function
	When a function calls another function, the local vars and the return address are pushed on to the stack
Frame pointer: Points to the start of the stack frame and stores pointer to the bottom of the frame (stored in %EBP)
![[Pasted image 20241116131347.png]]


### Stack overflow
Occurs when info is written onto a var in the stack, but the size exceeds the allocation (overflow) and the old BP gets overrideen
![[Pasted image 20241116131934.png]]

Stack will get overwritten which will redirect program exec (can cause segfault)
	Attacker can potentially run any code

Typical attack method: Overwrite return address of the current function with address of a different program, then when it returns, it will execute the attackers code with the priv of the original process


### How to do
The attacker needs to identify buffer overflow
Then they need to understand how the buffer works and then determine potential for corruption (by testing it)

The attacker will need to guess the location of the return address in the stack\
	He knows that the buffer exists but doesn't know the memory address of the buffer nor location of the stack FP
	In thoery, 2^32 possibilities for 32 bit system
To improve guessing, attacker can use *NOP sledding* which adds a ton of no operation instructions as entry points before the actual entry point of the code


SetUID will have the owners priv so
	If the setuid is set and owned by the root and has buff overflow, the attacker can act as root

Note: ANY code pointer can be attacked


### Causes
Caused by not checking buffer limit
Also caused by C stdlib functions that dont have bound checking
	strcpy
	strcat 
	scanf
	more

### Cures
Possible defense:
	Prevent exec of untrusted code
	stack canaries
	Encrypt pointers
	Address space layout randomization
	Code analysis (writing better code)

Executable space protection:
	Prevent execution of untrusted code by making all writable sections of memory non-executable 
	Requires hardware support: A special bit that deems whether you can exec or not
	Attacks can still corrupt stack or function pointers or heap

As long as RET points to existing code, ESP won't fix anything because it can still be overwritten


Stack protection:
	Add a secret (canary) between ret address and old base pointer
	This is a random string that can't be stored on the stack 
	Check the canary value before returning
	Can still be defeated if the attacker controls the value and the destination
	![[Pasted image 20241116191543.png]]


Use GNU extentions:
	Use gcc with additional function entry and exit code
	Func entry writes copy of return address to a safe region of memory
	Func exit checks the return address if it is the same as the one in the safe region, if it is not, abort

Address space randomization:
	Rand location of key data structures
	Rand order and virtual location of cstdlib
	Rand location of the buffers
	If randomized space is small, can just brute force

Other defenses: 
	Use safe languages
	Use safe libs
	Code analysis

# Other vuln
## Incomplete mediation
Mediation: Process of confirming authorization of a user before taking action
Incomplete mediation: Failure of checking completely and consistenyl
Occurs due to incorrect handling of input
	Input can be many types (hex, text, binary, urls, etc)

Can happen when passing params (loooong param in url or different type)

Integer overflow is when a int var goes over its limit causing it to wrap around


## Injection attack
Common in scripting languages 
Can be sql, code, or command injections

Code injection:
	When code inputted is executed by the system
![[Pasted image 20241116193720.png]]
	In this, PHP is assigning the global var (path) to the input of http 
	The included command can then be soruced 
	Protect:
		Block form field values to be assigned to global vars
		Use constant values in include, require, etc

Command Injection:
	Similar where a user input can be used to run a different command (or to run the same command in a malicious way)
	![[Pasted image 20241116200336.png]]
	To prevent it, add a test to make sure the input is safe

SQL injection:
	Get a program to output a malicious sql query by manipulating it
	![[Pasted image 20241116200539.png]]

Complete mediation validates all input, protects the input sources and assumes input of all types and sizes

# Race cond vuln
Time of check to time of use (TOCTTOU)
2 concurrent threads access the same resource at the same time
Can exploit because things can change between the time of check and time of use
![[Pasted image 20241116200735.png]]
The attacker can change fileX to something else after the check so that it can be written to
How easy:
	Timing has to be perfect but ways to extend the window like do IO
	Only has to get it write once and the attack can be ran over and over

To fix:
	Eliminate the window (mutex)
	Check multiple times making it harder
	Enable symlink prevention (that is how the attack is done)
	Reduce priv of program
