# OS services for user
- User interface- Almost all OS have some sort of UI
	- CLI
	- GUI
	- Touch screen
- Program execution- load in memory and a run a program
	- End it, normally or with error
- IO operations: interact with IO safely and effeciently 
- File system manipulation: provide access to mass storage
	- make, del, read, write dir/files
	- search file info
	- permission management
- Interprocess communication- exchange info among processes 
	- shared memory, POSIX (standard for design of UNIX OS)
	- message parsing, Microkernel, OS, RPC, COBRA
- Error detection: aware of possible errors
	- CPU and memory (power, fault)
	- IO (network connection, wire faulty)
	- user program (seg fault, div by 0)
# OS Services for efficient system operation
- resource allocation: provide access to shared resources in multiuser system
	- CPU cycles, main memory, file storage, IO
- Accounting: keep track of system resource usage
	- For cost accounting and usage stats
- Protection and security
	- Ensure access to system resources is controlled (protection)
	- Prevent outsiders from accessing system (security)
	- auth, file perms, address space restrictions
- ![[Pasted image 20240905141328.png]]

# Sys call
- Program interface to services in the OS
	- Request kernel mode service from the OS
	- Written in C/C++ usually (high level systems languages)
- Accessed by programs via API (high level) 
	- Simpler interface
	- Reduces coupling because syscalls are dependant on the system so the application is more portable
		- POSIX API (all unix)
		- Win 32/64 for windows (why there is a 32 and 64bit)
	- Implemented via trap
		- Register contains syscall number
		- syscall allows for the control to be transferred to the terminal fast
Example for copying 
![[Pasted image 20240905230502.png]]

![[Pasted image 20240905230524.png]]
printf calls write syscall
![[Pasted image 20240905230617.png]]

### Passing params
Pass additional info thru registers or stack (2)
1. Pass parms in registers 
	- simple, fastest, limit to 3
2. store args in a block on the stack and location is passed thru registers
3. params pushed on stack by program and popped off by OS
1 is hardly used because limit to 3, other 2 dont have a limit

### types of sys calls
- Process control
	- create/terminate process, get/set attributes, wait/signal event, allocate/free memory
	- Everything for C code
- File management
	- create, read, del, write, reposition, file attributes
- Device management 
	- request, release, read, write, reposition, device attr
- Info maintanance 
	- get/set time/date, get/set process/file/device attributes
- Communication
	- create/delete connection, send/recieve messages 
- Protection
	- set/get file/device perms, allow/deny sys resources
![[Pasted image 20240905232353.png]]

### Sys programs
User-level utility programs that come with the OS
	Makes it easier to m<mark style="background: #FFB8EBA6;"></mark>ake and execute jobs
	Not part of *kernel*
Sys programs are one of:
	File manipulation 
		mkdir, cp, rm, lpr, ls, ln, etc.
	status information 
		date, time, du, df, top, ps, etc.
	file modification 
		editors such as vi and emacs, find, grep, etc.
	programming lang support
		compilers, assemblers, debuggers, such as gcc, masm, gdb, perl, java, etc.
	program loading and execution 
		id
	communication 
		ssh, mail, write, ftp
	application programs
![[Pasted image 20240905232901.png]]
Linker links multiple files together to make an executable
Loader then loads the program into memory to run
# NOTE END FOR QUIZ
### OS design
Design
	Type of system: batch, time-shared, amt of users, distributed, real time, embedded
	User goals: convenience, ease of use, reliable, safe, fast
	sys goals: ease of design, implementation, maintanance, flexible, reliable, error-free, effecient
Mechanism 
	Policy: what to do
	Mechanism: how to do it?
Implementation:
	high level lang: easier, faster to write, easy to debug, portable
	Assembly: more effecient 
### Structure
OS needs to be modular, maintaibible, sustainable, 
Simple strcuture: 
	Characteristics
		Monolithic
		Poor seperation between interfaces and levels of functionality 
		Ill suited design, difficult to maintain and extend
	Reasons:
		Growth beyond original scope and vision 
		lack of neccessary hardware features when designing
		Guided by hardware constraints
	MS DOS
	![[Pasted image 20240906000610.png]]
	The application program (you) can access the drivers and lower levels
Traditional UNIX architecture:
	![[Pasted image 20240909142222.png]]
Layered:
	OS is divided into modular layers
	Upper layers use the functions that run in the lower layers
	Pros:
		 More modular, extensible, maintainable
		 Hides info! (higher level does not see what goes on in the lower level)
		 Simple to contruct, debug, and verify (if upper level throws bug look at the function it uses)
	Cons:
		Less effecient 
		It is harder to seperate functionality cleanly
	![[Pasted image 20240909142552.png]]
	The user can only access the layer beneath it
Microkernel:
	Make the kernel as small as possible by moving functionality into the user space
	Communicate between user modules (itself) using message parsing
	Pros:
		Easier to extend (just add more user level drivers)
		Easier to move to new architectures since the kernel itself is so small
		More reliable and secure (again since kernel is as small as possible)
	Cons:
		No consensus for services that should stay in the kernel and not
		Performance overhead because user talks to kernel (lots of context switching)
		