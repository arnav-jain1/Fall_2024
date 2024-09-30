# OS
Intermediate between user and hardware
	Easy and efficient execution 
Only specialized systems that have one task don't need an OS (baremetal)
## For users
OS abstracts things making it Easier to write programs, more powerful than ISA
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
	Memory is directly accessable by CPU using load/store
	Storage devices are not directly accessible and are like storage devices, must go through OS to access
OS
	Controls use of hardware amongst multiple resources
	Manages CPU, RAM, and IO (including HDD) 
	Kernel:
		Core component that runs with privilege 
	Instruction set architecture (ISA)
		User ISA: Accessible by all programs
		System ISA: Only accessible in privilege state
	System and application programs:
		System programs: programs that come with the operating system (like a compiler)
		Application program: program that the user creates or installs
			Define how system resources solve problems of the user (web browsers, video games)
	All programs must go thru OS to access the hardware, the cannot access directly
Users
	OS Manages security of multiple users
	OS respobsible for resource division being fair
![[Pasted image 20240828143500.png]]
### Startup
Bootstrap program to start computer (stored in ROM or motherboard)
	CPU will read specific address every startup
	Initialize CPU registers, device controllers, memory, locate and load OS kernel
	Gives control to OS
	Very difficult to write, especially for CISC like x86
BIOS: Basic input/output system:
	Stored in read only memory of the MoBo
	Contains the bootstrap
OS then starts executing the first program
	Each process called a daemon (starting with 1), located in memory
	waits for an event (interrupt driven)
Occurrence of an interrupt (exceptions/traps)
	Hardware driven (async events from hardware like keyboard) or software driven (like seg fault, div by 0)
	Steps
		processor checks for interrupt happening
		transfers control to interrupt service routine, usually through interrupt vector table (IVT, EECS388!)
			IVT in fixed address in memory that is known to the CPU
			Array of addresses where each address is a service routine
		Execute the associated interrupt service routine
		return control to the interrupted program	
	Interrupt service routine:
		Part of operating system that is placed in memory during the boot process
		Specific instructions to handle error
	OS manages the servicing of the interrupt (like transferring the input of the keyboard to the program)
	

OS Hides complexity and limitations of hardware (creates hardware interface). Simpler more, powerful abstraction (OS interface)
OS cycle:
	Wait for interrupt
	service interrupt when it occurs
	Back to waiting state

*End lec 1*
# Operating system types
## OS types
Batch systems:
	earliest OS
	user submits 1+ jobs in a *batch*
	OS collects a batch and executes each job one after another
	Good:
		Simple, no resource contention
	Problems:
		Sequential so slow
		I/O stalls the CPU because it has to respond to the IO which underutilizes the CPU
		No interaction
Multiprogramming
	Single job cant keep the CPU and IO busy 100% of the time so OS now gets multiple runnable jobs at once
	One one job has to wait, it switches to another job
	The original job is saved into memory (and jobs not being done)
	IO and CPU can overlap because some are CPU bound and others are IO bound
	OS Maximizes the CPU
	Limitation:
		Cannot handle infinite loops very well
Timesharing or multitasking
	Each program given a fixed time that it is allowed to run (ie 100ms)
	Once time is up, program is switched
	CPU switches jobs so often it can interact with each job that it's running
	very fast response time
	Allows for multiple users to work at once because:
		Each user has at least one program executing in memory
	OS scheduler decides which job will run if many are ready 
	Process doesnt fit in memory:
		Swapping: Move them in and out to run
		Virtual memory: Execute process not completely in memory
	OS goal: optimize user response time
	Drawback: May not achieve 100% util or highest throughput because constantly switching
Distributed systems:
	Physically seperate but networkly connected computers
	Hardware: computers with networks
	OS goal: Ease and optimize the resource sharing
Virtualization
	Additional layer between hardware and OS called hypervisor
	OS runs under the control of a hypervisor 
	hypervisor: 
		Creates multiple VMs by dividing hardware resources
		Allows diff OS to run on each VM
	Virtual machine controls the hardware and the OS runs on top of it
	VM has own memory management, file system, etc
	Key feature of some OS
	Hypervisor no longer optional
	Regained popularity because of cloud
# Tasks
- process coordination and security
	- Apps cant crash into each other
		- Address space: all addresses an application can touch
		- Seperated between each process
		- Segfault occurs when trying to access space it does not have access to
		- The code for the OS is mapped into the address space of every process so that the process can access OS functionality
	- Apps shouldn't crash OS
		- Dual mode: user and kernel
		- privileged instructions only operate in kernel mode
		- user programs run in lower privileged mode, OS in higher
		- Protects OS from illegal access
		- sys calls and interrupts change modes
		- Hardware support is required for this dual access mode, (example, DOS lacks hardware support for dual access)
	- Timer to prevent inf loop and resource hogging (multitasking)
		- OS sets a timer interrupt and when the timer hits 0, interrupt is generated
		- Set up before process starts to regain control and/or terminate once timer is up
![[Pasted image 20240905133245.png]]
- process management
	- Process = program in execution and unit of work,
		- Needs resources (termination requires reclaim of resources used)
	- Program is passive entity and process is active entity
	- Single threaded process has ONE program counter that specifies the location of the next instruction to execute
	- Multi threaded has one program counter PER thread
	- OS responsible for the following activities
		- process scheduling
		- suspend and resume processes
		- Mechanisms for process synchronization and communication
		- Mechanisms for deadlock handling (conflict handling)
	- Process instruction execution:
		- Usually sequential 
		- Control transfer instruction (jumps, returns, branches, function)
- memory management
	- Very limited (few gigs) so very important
	- All data in memory before and after process 
	- All instructions for process in memory in order 
	- Memory management determines what is in memory and when
		- Optimize util and computer response to users
	- OS responsibility: Manage memory
		- Keep track of what memory is being used and by who
		- decide which process/data to move in/out of memory
		- alloc and dealloc memory 
- storage management
	- Uniform, logical view of storage
	- Abstract the hardware properties to logicial storage unit (file)
	- OS responsibility: manage persistent storage (HDD/SSD)
		- View of all files and organization 
		- Privilege and access control
		- Create/del/manip files/dir (manipulation is like perms)
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
## IO Subsystem
One purpose of OS is to hide pecularities of hardware devices for the user
IO subsystem responsible for:
	Memory management of IO:
		Buffering
		caching 
		spooling
		drivers
Protection:
	Controlling access to processes/users 
	Distinguish what users can do what
	user identities include (user ID, security ID), one per user
		Determines what user can access what files
	groupID allows a group of users to be defined and have their perms collectively managed
	Privilege escalation allows users to change IDs to have more access (or less)
Security:
	Defense of system against external and internal attacks
	DDOS, worms, viruses, etc

# Summary
OS is a layer between hardware and user
Essentially the GOAT API
OS is the first program that loads upon boot, and it is always running 
It manages resources and is the creator of a virtual machine
![[Pasted image 20240905135350.png]]