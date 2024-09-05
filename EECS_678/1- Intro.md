# OS
Intermediate between user and hardware
	Easy and efficient execution 
Only specialized systems that have one task don't need an OS (baremetal)
## For users
Easier to write programs, more powerful than ISA
Makes it easier to run programs (like loading program into memory, initialize program state and maintain a program counter, stopping)
	Memory is what process can directly access
security, resource management, performance, multi user mode

## For systems
Allocates resources
	Manage CPU, memory, files, IO, network, etc.
	Resolve conflicts from multiple programs wanting to use the same thing
control IO devices
## Computer system
Hardware 
	Basic computing resources (Memory, IO, CPU, etc)
OS
	Controls use of hardware amongst multiple resources 
Application programs
	Define how system resources solve problems of the user (compilers, web browsers, video games)
Users
![[Pasted image 20240828143500.png]]
### Startup
Bootstrap program to start computer
	Initialize CPU registers, device controllers, memory, locate and load OS kernel
OS then starts executing the first program
	Each process called a daemon (starting with 1), located in memory
	waits for an event (interrupt driven)
Occurrence of an interrupt (exceptions/traps)
	processor checks for interrupt happening
	transfers control to interrupt service routine, usually through interrupt vector table (IVT, EECS388!)
		IVT in fixed address in memory
	Execute the associated interrupt service routine
	return control to the interrupted program	

Hides complexity and limitations of hardware (creates hardware interface). Simpler more, powerful abstraction (OS interface)

On boot, bootstrap locates and loads OS into memory
OS waits for events

# Operating system types
## OS types
Batch systems:
	earliest OS
	user submits 1+ jobs in a *batch*
	OS collects a batch and executes each job one after another
	Problems:
		Sequential so slow
		I/O stalls the CPU because it has to respond to the IO which underutilizes the CPU
		No interaction
Multiprogramming
	Single job cant keep the CPU and IO busy 100% of the time so OS now gets multiple runnable jobs at once
	One one job has to wait, it switches to another job
	The original job is saved into memory
	IO and CPU can overlap because some are CPU bound and others are IO bound
	OS Maximizes the CPU
Timesharing or multitasking
	CPU switches jobs so fast it can interact with each job that it's running
	very fast response time
	Each user has at least one program executing in memory
	OS scheduler decides which job will run if many are ready 
	Process doesnt fit in memory:
		Swapping: Move them in and out to run
		Virtual memory: Execute process not completely in memory
	OS goal: optimize user response time
Distributed systems:
	Physically seperate but networkly connected computers
	Hardware: computers with networks
	OS goal: Ease and optimize the resource sharing
Virtualization
	OS runs under teh control of a hypervisor 
	VM has own memory management, file system, etc
	Key feature of some OS
	Hypervisor no longer optional
# Tasks
- process coordination and security
	- Apps cant crash into each other
		- Address space: all addresses an application can touch
		- Seperated between each process
	- Apps shouldn't crash OS
		- Dual mode: user and kernel
		- privalged instructions only operate in kernel mode
		- sys calls and interrupts change modes
	- Timer to prevent inf loop and resource hogging
		- OS sets a timer interrupt and when the timer hits 0, interrupt is generated
		- Set up before process starts to regain control and/or terminate once timer is up
![[Pasted image 20240905133245.png]]
- process management
	- Process = program in execution and unit of work,
		- Program is passive entity and process is active entity
		- Needs resources (termination requires reclaim of resources used)
	- Single threaded process has ONE program counter that specifies the location of the next instruction to execute
	- Multi threaded has one program counter PER thread
	- OS responsible for the following activities
		- process scheduling
		- suspend and resume processes
		- Mechanisms for process synchronization
		- Mechanisms for process communication
		- Mechanisms for deadlock handling (conflict handling)
- memory management
	- All data in memory before and after process 
	- All instructions for process in memory in order 
	- Memory management determines what is in memory and when
		- Optimize util and computer response to users
	- Manage memory
		- Keep track of what memory is being used and by who
		- decide which process/data to move in/out of memory
		- alloc and dealloc memory 
- storage management
	- Uniform, logical view of storage
	- Abstract the hardware properties to logicial storage unit (file)
	- File system management 
		- Privaledge
		- Create/del/manip files/dir
		- mapping files onto secondary storage
		- Backup files onto stable storage media (HDD, SSD)
- Protection and security
	- Protection: Mechanism for controlling access resources from  processes or users 
	- Security: Defense of the system against internal and external attacks
		- DDOS, worms, viruses, identity theft
	- Systems first distinguish which users can do what
		- userID inc name and a number, one per user
		- userID is associated with all files, processes of that user can determine contor
		- groupID allows a group of users to have their permissions managed together
		- Privilege escalation allows user to exchange ID to one with more rights

## IO subsystem
OS hides peculiarities of hardware devices from the user
Responsible for 
- Memory mangement for IO
	- Buffering: Store data temp while being transferred
	- Caching: Store parts of data in faster storage for perf
	- Spooling: Overlap of output of one job to input of other
- Device-driver interface
## Storage perf
Depends on distance from CPU
Movement can be implicit or explicit
![[Pasted image 20240905134342.png]]

# Summary
OS is a layer between hardware and user
Essentially the GOAT API
OS is the first program that loads upon boot, and it is always running 
It manages resources and is the creator of a virtual machine
![[Pasted image 20240905135350.png]]