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
OS sits between hardware and user program
Blue part is the OS
ISA is how the OS interacts with hardware

Kernel is everything below the syscall layer and has a higher privilege 

# Sys call
- Only way to access hardware
- Program interface so that users can use services in the OS
	- Implemented as part of the kernel
	- Request kernel mode service from the OS (higher priv, must switch)
	- Written in C/C++ usually (high level systems languages for performance and hardware access)
- Accessed by programs via API (high level) 
	- Simpler interface, written as libraries like libc
	- Reduces coupling because syscalls are dependant on the system so the application is more portable
		- POSIX API (all unix)
		- Win 32/64 for windows (why there is a 32 and 64bit)
	- Implemented via trap
		- Register contains syscall number
		- syscall allows for the control to be transferred to the terminal fast
- Why syscall has high overhead
	- Switch between user to privileged mode
	- Inturrupt mechanism 
	- save and restore registers
	- Actual example: get_id (user level) vs get_pid (system level)
		- Loop this and run it with the time command
		- Syscall one takes longer
	- Still faster than interrupt
Example for copying :
	Getting input from keyboard (file name)
	Creating a new file
	OPening the old file
	Reading old file and writing to new file
	Abort on error
![[Pasted image 20240905230502.png]]

![[Pasted image 20240905230524.png]]
printf calls write syscall
![[Pasted image 20240905230617.png]]

# Debugger  and preprocessor (not on slides)
- Compile with -g flag 
- Use gdb to trace execution 
	- Break points to check program state
- Can also check assembly instructions using -s
- You move the syscall number to the right register (each syscall has its own number)
- 450 total
- Sys call table:
	- listing of all sys calls with their number
	- Used by kernel to start the right routine
- Sys calls are more expensive due to mode switch and context switch

### Passing params
Pass additional info thru registers or stack (2)
1. Pass parms in registers 
	- simple, fastest, limit to 6 on x86-64, rest on stack
2. store args in a block on the stack and location is passed thru registers
	- x86-32 
3. params pushed on stack by program and popped off by OS
1 is hardly used because limit to 3, other 2 dont have a limit
Combination most common

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
