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
![[Pasted image 20240905135350.png]]# OS services for user
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
# Process
Process aka job:
	Program in execution
	Instance of a program that is being executed sequentially 
	Must be executed sequentially
Program
	passive entity, not being executedi
	Only contains text
		Code, data, PC, heap, stack, registers
	Programs become processes when loaded into memory
	Same program executed multiple times are different processes
![[Pasted image 20240911141904.png]]
Process address space:
	Abstraction of memory resource for a project
	4 segments:
		Stack is automatic, local vars and is only there during the function activation and storing
			Allocated when function is entered then dealloced when done
			Unbounded recursion can caused stack overflow. 
			Reaching outside of the allotted space on the stack will cause segfault
			Stack size max: 8MB, can grow if neccessary 
		Heap is for dynamically allocated data, explicit or implicit (depending on language), scope continues until deallocation. Can grow/shrink
		Data: global and static data, allocated/deallocated on process creation/termination, scope is during execution. Automatically initialized to 0
		Text: Program binary instructions (sequential instructions from assembly/C) with same properties as data
	Using size ./program you can see how much for each
	BSS is uninitialized global vars. dec is the sum and hex is the hex representation
	![[Pasted image 20241001190907.png]]
![[Pasted image 20240911142409.png]]
Data and text dont change. Stack moves down per function call and heap is for dynamic allocation

There is a limit to how much space you are allowed on the stack (you can change it if you want

Memory mangement responsibilities:
	OS: Provide memory space for the process
	Compiler: Manage stack allocation
	Language runtime/memory allocator: Manage heap
	Program: Determine how memory is used
Memory management strats:
	Explicit: Programmer handles
	Implicit: Language handles (Python/garb collector)

	

Process states:
- New: Process being created
	- New -> ready: Process loaded into memory
- ready: waiting to be processed by CPU (in memory)
	- Ready -> Running: Scheduler dispatches process to CPU
- running: instructions being executed
	- Running -> Terminated: Process completes execution
	- Running -> Waiting: Process starts a blocking IO operation so waits till done
	- Running -> ready: Time allotted (quantum) runs out
- waiting: Process is blocked, waiting for the event
	- Waiting -> ready: IO operation completes
- Terminated: Finished executing, process cleanup occuring
![[Pasted image 20240911143809.png]]
Time quantum:
	Time sharing system:
		Each process allotted fixed time on the CPU
		Process interrupted and moved to ready if time expires (running -> ready)
		Unused time is lost if the process reaches waiting early
	Multiprocessing:
		May not have running -> ready
		Process continues till completion or waiting (IO)
CPU: One core can run one process at a time, multicore can do multiple
Memory allocation for ready processes: Address space allocated (including text, data, heap, stack) where heap and stack may be empty initiially

### Process control block
Representation maintained by OS for each process
Has important info
	Process state
	Program counter
	CPU registers
	Scheduling info
	Memory management info
	Accounting info (CPU usage, time limit, etc)
	IO information (Open files, devices in use)
Important because it allows the OS to track and mange processes and enables context switching

Implemented with **task_struct** in the linux kernel
# Context switch
Process of storing and restoring the state (*context*) of CPU so that multiple processes can share a CPU
	Time shared, multiprogramming env
	*Context* represented in PCBi
	Save current process then restore the next process
	Switching from user <-> kernel is a mode switch
	*Updates accounting information and other PCB data*
Overhead because no other work is done when switching (OS tries to minimize for effeciency)

![[Pasted image 20240911165905.png]]
The process scheduler decides which process to run
	Goal is to max utilization in multiprogramming OS
	Provides illusion of each process owning the system in timeshared OS
Terminology:
	Job queue: all processes in system
	Ready queue: all processes in memory waiting to be executed
	device queue: set of processes waiting for IO device
Processes can move between queues
![[Pasted image 20240913142209.png]]

![[Pasted image 20240913142228.png]]

Systems with huge job queue can have multiple schedulers
Long term scheduler
	Selects processes to be brought into ready queue (memory)
	Controls degree of multiprogramming
	Controls the mix of CPU bound and IO bound processes
	more time to make selection, so called less frequently
	Only in very specialized cases like cluster nodes
Short term scheduler

## Process creation
Process can make other processes
	OS has *primordial* process (pid of 1)
	creating process is *parent* while created process is *child*
		If parent dies before child, the child process becomes an *orphan* 
	Processes IDed by process identifier PID 
Resource sharing options
	1. Parent and child shares resources 
	2. Children share subset of parent process resources
	3. Parent and child doesn't share resources (Unix, exact duplicate)
Execution options
	1. Parent and child run together (unix)
	2. Parent waits till children finishes 
	Parent process has to collect the child process status when the child process is finish
		If it doesn't collect it, the child process becomes a zombie process
	If Parent process finishes first, it will either be orphaned or zombied
	All the parent's responsibility is to collect the status of the child when done
Address space 
	child duplicate of parent ( unix duplicated but different address space, no shared memory)
	Text, data, stack, heap of the two processes are sperate but equal
	child has a program loaded into it by exec, the rest gets overridden (if succeed)
	The two address spaces have protection, cannot access each others
	Note: registers also duplicated
UNIX examples
	Fork system call will create a new process
	Exec will replace the process' memory space with a new program

Parent waits for its child to finish
![[Pasted image 20240917175545.png]]
Important notes when reading code:
	If value is assigned to output of fork like int val = fork(); then, the parent will have the child PID for its value and the child will have 0
		If it is -1, then it failed
	If else block used to differenciate parent and child
	Exec deletes the rest of the code (if succeed)
	Wait is used to pause until completion to recieve the exit status
	Parent can terminate without child being terminated (child parent becomes 1 or systemd)
	Execution of child starts after the fork() call because register/Program counter
	getpid() returns process id
	getppid() returns parent procces id (1 if no parent cause systemd)
	execution without wait() might lead to interleaved output
```c
int main() {
	int val = fork();
	if (val < 0) {
		printf("Failed");
	} else if (val == 0) {
		printf("child");
	} else if (val > 0) {
		printf("Parent");	
	}
}
```
Also, Multiple forks make a tree

## Process termination
Process ends after the last statement executes
	The process itself can explicitly call **exit()** but if it doesn't, then the OS implicitly calls it.
	Child can then pass it's return status to the parent that collects it using **wait()**
	Resources are then dealloc by OS (**AFTER** process is done, not after function is done)
Zombie process:
	Processes that are done but exit status hasn't been collected
	Remain in system till exit status collected
	Only take up a small amount of system resources, so not the end of the world
Parent can terminate the child process explicitly using **abort()**
	Like if child is using too many resources or is not needed
	Needs PID
	No easy way to terminate all at once, individually is easier
Is parent exits then:
	Child becomes orphan process
	The child will be assigned a new parent by the OS and can run independently  (Unix)
	Might not be the case for other OS

#### Sidenote: Chrome
Chrome is mutliple processes 
	Browser: Manages UI 
	Renderer: Renders web pages
	Tabs: each tab is own process (same with plugins)
Good because makes it harder to crash
Bad because more utilization

## Inter process communication
How to communicate within the same system 
	Needed to share info, speed up computation, more modular, convenient 
	Global vars does NOT work because entire address space is copied

Producer makes the info and consumer process the info
This needs to be synchronized though because consumer needs producer to be finished first

Abstraction models
	Unbounded buffer: no (practical) limit on size of buffer so producer can produce how much every they want 
	bounded buffer: limit on size of buffer, producer cant produce more until buffer is freed a little

## IPC models
For transfer data, same task but seperate
	Info sharing
	Speed up computation (multicore)
	Modularity
	Convencience 

Shared memory:
	Requires overwriting address space protection for one specific region
	Processes can then read and write to it like it is malloced, memory address is shared
	Memory address is shared and can be read/written to 
	Fast, convinient  (sys call not needed for write/read)
	Can potentially overwrite one's data, also requires synchronization
Message parsing
	Send and receive messages via queue (located in kernel space, involves copying data from user to kernel space)
	Messages not overwritten so no conflicts
	Slower but better for multiple computers
	Easier to implement 
	Typically used for smaller amounts of data
	send() and recieve() (sys calls)
![[Pasted image 20240918173407.png]]

Message parsing can also be used for client-server communication 
2 operations: send and receive (a message)
If P and Q want to communicate, they need to first establish a link before exchanging messages
Implementation issues (producers vs consumer):
	How to establish a link?
	Can a link work with >2 processes
	How many links between communicating processes (1 for 2, 2 for 2)
	Max capacity of a link? (how many/how big of messages)
	is the message length fixed or variable size? 
	Is the link unidirectional or bidirectional 

### Naming
Direct
	Message must be named directly (aka send(Q, message) and recieve(P, message))
	There is exactly ONE link between exactly TWO processes and it is established automatically
	Issue: Process IDs are hard coded, so you need the PIDs which can change

Indirect:
	Messages are sent to and recieved from ports (mailboxes)
		send(A, message) puts the message in mailbox/port A
		recieve(A, message) receives from mailbox A
	Each mailbox has a unique ID
	Processes can only communicate if they share a mailbox 
	Allows for many processes, many links, bi/unidirectional but may need syncronization
	Mailbox in address space or kernel

Syncronization issue:
	Message receiving/sending is either blocking (sync) or nonblocking (async)
	**Blocking send**: Sender cannot continue until successfully sent (so if the buffer is full, it won't do anything until it is able to send it). ensures the message was recieved
	**Blocking recieve**: Cannot continue until successfully recieved
	**Non-blocking send**: Sends the message and then continues
	**Non-blocking recieve**: recieves the message or null and then continues
Buffer: queue of messages 
	0 capacity: Sender always blocked until reciever gets the message, very strict synchronization 
	Unbounded capicty: impractical but means senders will never block 
	Bounded capacity: At some point the sender will block because there will be no space

## Modes

Pipes, FIFOs (named pipes), message queues, shared memory, and sockets

### Pipes
Most important (like | in command line for output of one command becomes input of second)
Most basic form of IPC
Way for two processes to communicate ONLY in a parent-child relationship (because unnmaed)
![[Pasted image 20240923180905.png]]
(fd is a file descriptor fd\[0] is receive while fd\[1] is send
Only one direction, you write from one end and then read from the other

They are anonymous (no name)
Only in related processes on the same OS
Implemented in kernel
Bounded capacity buffer
Pipe *before* forking

Issues/info:
	Unidirectional
	Only constructed in parent-child or child-child relationship
		Only exist until the process exists so if the process terminates early, data might be lost
		AUtomatically deleted when process ends
	Must be controlled by same OS (cant be used over a network)
	Data only FIFO
	
fd0 is stdin, fd1 is stdout and fd2 is stderr so when you create a new pipe. the file descriptors for reading for the pipe is fd3 and writing is fd4
read is 0 and write is 1

When you create a fork after already making file descriptors, the child gets the same one so then the child can write to it and the parent can read from it

What does | do? 
	example: /bin/ps -ef | /bin/more
		Create a process to run `ps ef`
		Create a process to run `more`
		Create a pipe from `ps ef` to `more`
		The stdout of `ps ef` is redirected into the pipe
		The stdin of `more` is the output of the pipe

File table:
	OS has a table of all open files (inc stdin, stdout, stderr)
	Each process has own fd table that points to entries in the OS fd table
	Child gets copy of the parent table

Process synch:
	Pipes used to synchronize and control order of what to run. 
	Since reading is blocking, you can make one process run after another by having the second process read and the first one write when done
## FIFO (named pipes)
Pipes with a name
More powerful than anonymous (regular) pipes
	No parent-sibling relationship needed (works with unrelated processes)
	Bidirectional 
	Can persist after process terminates
mkfifo to make a fifo
open() blocks until a reader/writer is availible
when writers close, read returns 0
Characteristics:
	Appear as special files (not file table)
	half duplex
	communication must be on same machine and file system but more flexible
	file is 0 bites big
	Persists until deleted so make sure you delete it manually

Sender and receiver can't be the same (can't open read and write in same process)
Data retrieval is STRICTLY fifo
Multiple recievers gets fuzzy

```c
#define FIFO_NAME "/tmp/myfifo"
#define BUFFER_SIZE 256
int main() {
    int fd;
    pid_t pid;
    char buffer[BUFFER_SIZE];

    // Create the named pipe (FIFO)
    mkfifo(FIFO_NAME, 0666);

    pid = fork();

    if (pid == 0) {  // Child process (writer)
        printf("Child: Opening FIFO for writing...\n");
        fd = open(FIFO_NAME, O_WRONLY);
        
        const char *message = "Hello from child process!";
        printf("Child: Writing message: %s\n", message);
        write(fd, message, strlen(message) + 1);
        
        close(fd);
        printf("Child: FIFO closed.\n");
        exit(0);
    } else {  // Parent process (reader)
        printf("Parent: Opening FIFO for reading...\n");
        fd = open(FIFO_NAME, O_RDONLY);

        printf("Parent: Reading from FIFO...\n");
        ssize_t bytes_read = read(fd, buffer, sizeof(buffer));
        
        if (bytes_read > 0) {
            printf("Parent: Received message: %s\n", buffer);
        } else if (bytes_read == 0) {
            printf("Parent: End of file reached.\n");
        } else {
            perror("read");
        }

        close(fd);
        printf("Parent: FIFO closed.\n");

        // Wait for child to finish
        wait(NULL);

        // Remove the FIFO
        unlink(FIFO_NAME);
    }

    return 0;
}
```
## Message Queues
Aims to provide more control when sending to multiple receivers

Linux uses indirect communication or mailboxes
There can be multiple processes with queues (sychronization may be needed)

Processes can use any number of queues (each queue is unique)
Capacity of the link is initialized by the system (can be overridden by user)
Each message has a length which is specified in send and receive calls
Each process can send and recieve calls from the same queue

Message queues is how Linux sends and receives messages
	First 4/8 bytes is the message type
	Rest is the content
	Also need to provide the buffer and size

msgget: Create a new message queue
msgsnd: Send a message to the queue
	struct msg_buf { long mtype; char mtext\[]}
	Non blocking unless queue is full
msgrcv: Receive message from queue (mtype to get specific messages)
msgctl: Control operations for the queue (like terminating)

You have to specifiy the queueID 

Note:
	Limited security, as long as you have the queue ID you can access
	Message removed when recieved
	Created in kernel space, requires syscalls
	May miss if not coordinated

Can also create a key using ftok()
Great for multiple senders/recievers, the type allows specific on who to recieve

```C
struct msg_buffer {
    long msg_type;
    char msg_text[MSG_SIZE];
};

void message_passing_example() {
    int msgid;
    struct msg_buffer message;
    pid_t pid;

    // Create message queue
    msgid = msgget(IPC_PRIVATE, 0666 | IPC_CREAT);
    if (msgid == -1) {
        perror("msgget");
        exit(1);
    }

    pid = fork();

    if (pid < 0) {
        perror("fork");
        exit(1);
    } else if (pid == 0) {  // Child process
        message.msg_type = 1;
        strcpy(message.msg_text, "Hello from child process!");
        if (msgsnd(msgid, &message, sizeof(message.msg_text), 0) == -1) {
            perror("msgsnd");
            exit(1);
        }
        exit(0);
    } else {  // Parent process
        wait(NULL);
        if (msgrcv(msgid, &message, sizeof(message.msg_text), 1, 0) == -1) {
            perror("msgrcv");
            exit(1);
        }
        printf("Message Passing: %s\n", message.msg_text);

        // Remove message queue
        msgctl(msgid, IPC_RMID, NULL);
    }
}
```
## Memory sharing
multiple Processes can utilize the same chunk of memory (Same OS only)
Implementation principles:
	Name unique (system wide) or anonymous 
	Specifying permissions (read, write, not execute on linux)
	Dealing with race conditions (atomic or synchronized access)
	Most thread level communication is from shared memory
Example:
	shmget: create shared memory segment 
		Requires size and returns identifier
		Same access perms as files
	shmat: attach shared memory segment (returns void* so cast it)
		Must for every process that wants to access it
		Identified by segment id
	shmdt: Detach shared memory
	shmtcl: Control operations (like removing)
Shared memory remains until removed (view using ipcs)
```C
void shared_memory_example() 
{ 
	int shmid; 
	char *shared_memory; 
	pid_t pid; // Create shared memory segment 
	shmid = shmget(IPC_PRIVATE, SHM_SIZE, IPC_CREAT | 0666); 
	// Attach shared memory segment 
	shared_memory = (char *)shmat(shmid, NULL, 0); 
	pid = fork(); 
	if (pid == 0) { 
		// Child process 
		sprintf(shared_memory, "Hello from child process!"); 
		
		strcpy(shared_memory->message, "Hello from child process!");
		shared_memory->message_ready = 1;
		exit(0); 
	} else { 
		// Parent process 
		wait(NULL); 
		printf("Shared Memory: %s\n", shared_memory); 
		while (shared_memory->message_ready == 0) { 
		// Wait for child to send message 
		} 
		printf("Parent: Received message: %s\n", shared_memory->message);
		// Reset the message_ready flag 
		shared_memory->message_ready = 0;
		
		// Detach and remove shared memory segment 
		shmdt(shared_memory); 
		shmctl(shmid, IPC_RMID, NULL); 
	} 
}
```
Notice no syscall for read/write


## Unix sockets
Sockets:
	End point for communication 
	Two way communication pipe
	Variety of domains like the internet
Unix domain sockets:
	Communication between processes on same Unix system
	Special file in the system (like FIFO)
Mostly for client-server programming
	Client sends a request for info (like API calls)
	server waits for requests then does the request and sends info (updates, output) to client
Modes:
	Connection-based: TCP
		Heavyweight, makes sure that the packets are in order with none lost
		Slow
	Connection-less: UDP
		Faster, does not care for order/drops
		Better for programs that don't care about packet loss (streaming, games)
Syscalls:
	socket(): make the Unix socket
		int socket(int domain, int type, int protocol)
		Domain = AF_UNIX for unix domain sockets
		type specifies Connection based or connectionless 
	bind(): Assign an address to a socket int bind(int sockfd, ... \*addr, adderlen)
		Filepath is used as address for the socket
		makes A file. also what you connect to when you go to a website
		Connects the socket itself to a file
	listen(): listen to incoming client requests
		int listen(int sockfd, int backlog)
	accept(): Create a new connected socket
	send():
	recv(): Receive messages from socket (message placed in buf)
	close(): Closes the connection
	(Not read/write)
Socket file descriptor:
	Single file for read/write
Unix vs Internet:
	Unix uses file names as addresses
	Internet uses IP addresses

Server vs client:
	Server has to use socket() listen() and accept() (and bind)
		Server handles multiple clients by making a new fd for each connection
		backlog in listen() specifies max number of pending connection
	Client just uses connect() to connect to a server

```c
#define SOCKET_NAME "/tmp/example_socket"
#define BUFFER_SIZE 256
int main() {
    int server_fd, client_fd;
    struct sockaddr_un server_addr, client_addr;
    char buffer[BUFFER_SIZE];
    pid_t pid;

    // Create socket
    server_fd = socket(AF_UNIX, SOCK_STREAM, 0)

    // Configure server address
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sun_family = AF_UNIX;
    strncpy(server_addr.sun_path, SOCKET_NAME, sizeof(server_addr.sun_path) - 1);

    // Remove any existing socket file
    unlink(SOCKET_NAME);

    // Bind socket to address
    if (bind(server_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) == -1) {
        handle_error("bind");
    }

    // Listen for connections
    if (listen(server_fd, 1) == -1) {
        handle_error("listen");
    }

    pid = fork();

    if (pid == 0) {  // Child process (client)
        sleep(1);  // Ensure server is ready

        int client_socket;
        client_socket = socket(AF_UNIX, SOCK_STREAM, 0) 

        connect(client_socket, (struct sockaddr*)&server_addr, sizeof(server_addr) 
        const char *message = "Hello from child process!";
        send(client_socket, message, strlen(message)

        printf("Child: Message sent.\n");

        close(client_socket);
        exit(0);
    } else {  // Parent process (server)
        printf("Parent: Waiting for connection...\n");

        socklen_t client_addr_len = sizeof(client_addr);
        client_fd = accept(server_fd, (struct sockaddr*)&client_addr, &client_addr_len)

        ssize_t bytes_received = recv(client_fd, buffer, BUFFER_SIZE - 1, 0);

        buffer[bytes_received] = '\0';
        printf("Parent: Received message: %s\n", buffer);

        close(client_fd);
        close(server_fd);

        // Wait for child to finish
        wait(NULL);

        // Remove the socket file
        unlink(SOCKET_NAME);
    }

    return 0;
}
```
# Process overview/recap
Basic unit of CPU scheduling is a process
To execute instructions and run a program, create a process
Properties:
	fork() -> exec() to start new execution
	Processes well protected from each other
	Context switching is expensive
	IPC to share info (message passing or shared memory)

Process not always suitable. Stuff you might want:
	You want the ability to control multiple sequences (threads) of control but process only allows 1 thread of control
	Need a way for threads to share data better
	Protection between threads doesn't matter
	Switching between threads needs to not have too much overhead
	Different threads may share most info (duplicate address space)

Threads can do all of this and are known as lightweight processes
	Only important info is regend
		Thread context: PC, registers, stack
		Process context: $\uparrow$ + data, heap, code
	Smaller context so faster switching
	threads are executed within a process
		Code and data shared so less communication overhead
	Single address space for all threads

Approaches to multiprogramming:
	Monolithic process:
		One program handles all objects
		Bad for multicore, slow
	Multiple processes:
		High overhead (in a game, each object would be own process) 
		Context switching would be a nightmare
		Sharing data would be worse
# Threads
Lightweight process with:
	Independent flow of control
	but still shares resources with other sibling threads
	Lives in the same context space as the process
	parallel execution, uses multiple cores more effeciently
Threads NOT protected from each other, can access entire address space
Partitioning managed by OS/library not compiler
	Created far enough to avoid conflicts (at least for now)

Thread shared data:
	Process instructions
	most data (the global vars part of address space that are initialized to 0)
	File descriptors (if one closes, closed for all)
	signals and signal handlers 
	current directory
	user and group id
Thread specific data:
	Thread ID
	registers, stack pointer
	thread-specific data (local vars)
	stack
	signal mask
	scheduling properties
	return value
![[Pasted image 20241002142032.png]]

Thread pros:
	Responsiveness for user if part of the application is blocked
	Easier to share resources (global vars) but need to be aware of sync issues
	Economy: Reduces creation, context switch, and space overhead
	Scalability: Better utilizes multicore CPU

Created in Pthreads library
	IEEE POSIX C interface

Pthreads API functions:
	thread mangement (create, destroy, detach, etc)
	Mutexes: functions that force synchronizations (like for counters) 
	Condition vars: Manage thread communication (create, destroy wait depending on variable values)
	Funcs:
		pthread_attr_init: Initialize thread struct
		pthread_create: create a new thread
		pthread_join: wait for thread to exit (like join up with thread, blocked until done)
		pthread_exit: terminate the thread


User level threads: Manges the thread at the user level
	Pros:
		Fast and flexible in space, speed, switching, and scheduling
		No syscall needed
	Cons:
		One blocked thread can cause all threads to be blocked
		OS doesn't really know what is going on so it allots the same time and space
		Popular before 2005 because CPUs were all single core, now if thread is blocked, kernel will only see one process so it is hard to take advantage of multiple cores
Kernel level threads: Thread at kernel level
	Pros: 
		Removes disad of user level
		supported by OS so individual thread scheduling
	Cons:
		More overhead because syscall needed (and context switch)
	Provided by all OS (Mac, windows, linux)

# Multithreading models
Many-to-one
	Many user level threads mapped to a single kernel level thread
	Bad because if one user thread is blocked, so is the rest of the threads
	Bad for multicore CPUS
	only used when OS doesnt support threads
One to one model
	Each user level thread has a kernel level thread
	What pthreads is 
	Good because if one thread is blocked, the user level thread is not
	great for multicore
Many to many model
	m user threads to n kernel threads
	Used for large scale application because it reduces overhead of creating one and you can just map the threads as needed
	Complex
![[Pasted image 20241002144446.png]]

Two level multithreading model:
	Similar to M:M but still allows one to one
![[Pasted image 20241002144432.png]]


Threading issues:
	semantics of fork() and exec()
		Does fork duplicate all of the threads or just the spawning thread? Will child have the same number of threads?
			For linux, no. Only the thread that calls fork is duplicated
			For other OS? Maybe
		WIll exec override the entire address space or just the calling thread?
			Linux: Entire address space (ALL threads) overrwitten
		Only reason to do this is to call fork and exec right after so the child process runs exec


END SLIDE 21# Quiz 1
![[Pasted image 20241006104950.png]]
## Q 3:
![[Pasted image 20241006105236.png]]
1. T, many jobs can be active
2. F, one at a time
3. T, while waiting for input, does other things
4. Nope, has to finish
5. T, multiprogramming maximizes throughput while multitasking maximizes user response time
6. F, m

## Q 4
The OS uses the address space protection mechanism to:
	ensure applications do not crash into each other
	    T
	ensure applications do not crash into the OS
		F
		*This is why there is a dual mode*

## Q 5
The memory location of the interrupt vector table (IVT) must be known to  the OS, but not necessarily to the CPU.
	False
	OS tells CPU so CPU can find it, they both know

## Q6
After startup, the operating system is idle until there is an interrupt.
	False
	*OS waits*


# Quiz 2
![[Pasted image 20241004141539.png]]
1. syscall is faster than trap because syscall is simpler and designed to be effecient
2. Yes because stack is used as well
![[Pasted image 20241004141626.png]]
3. Yes because it uses a syscall to get it, if it didnt then it would not cause a mode switch
4. Yes, fork()
![[Pasted image 20241004141721.png]]
5. Obv
6. System programs are programs shipped *with* the OS, A-D are GCC. Loader is part of the OS. Another way to think about it is that you can remove system programs, you can't remove a loader
![[Pasted image 20241004142145.png]]
7. True and true(why you do sudo apt install)
8. Text: where binary code there is so thats where the cpu is reading the program from. Data: Global and static vars. Both of these are known at compiletime. Stack and heap can change during runtime
# Quiz 3
![[Pasted image 20241004142406.png]]
1. Entire address space is cloned but the PID has to be different

![[Pasted image 20241004142451.png]]
2. 1. Running to wait is IO or event wait 2. IF it uses its time slice it goes back to ready so interrupt

![[Pasted image 20241004142719.png]]
1. Because the parent is waiting the child can wait
2. Printed 0 times
3. fork after begin so only once
![[Pasted image 20241004142920.png]]
1. for the parent rvalue is child ID (20) and for the child process it is 0 so False
2. True
![[Pasted image 20241004143032.png]]
1. False, only cleans when program finishes
2. Register state saved in memory

# Quiz 4
![[Pasted image 20241004143509.png]]
![[Pasted image 20241004143546.png]]
1. a
2. False, reading or writing to shared memory does not need syscalls, attaching does
3. True shared memory is faster bc less syscall
4. Technically yes (example in the slides)
5. F it blocks
![[Pasted image 20241004143614.png]]
1. True, makes a FIFO file
2. False, it can only access one end

![[Pasted image 20241004143708.png]]
7. pipe becomes new stdin (stdin is 0 and stdout is 1). Close closes old stdin
![[Pasted image 20241004143815.png]]
1. Need to open a pipe
2. Nothing yet
3. Then you need to send a signal to the pipe so write to 1 (1 is stdout)
4. wait for std in so read from pipe (0 is stdin)
5. Then You are done so do nothing or close
6. Again do nothing# Chapter 1  
1. **Explain the User’s and System’s view of the operating system.**  
User: Abstracts many things making it easier to write and run programs
For CPU: Manages memory, IO, CPU and resolves conflicts

2. **Explain the operating system goals.**  
Make it easier for the user to do what they are trying to do, manage resources, have multiple users, incorporate security 

3. What is a multiprogramming OS? How does it differ from a batch OS? How does it differ from a time-sharing OS?  
Multiprogramming has multiple processes loaded into memory that are switched in and out to maximize CPU effeciency. Batch does one after another so if it gets stuck on a low CPU usage task that it can't get through (like IO), then it just stays there. Timesharing allots a specific amount of time for each process. This is done because multiprogramming struggles with infinite loops but that is not a problem for timesharing

4. How does an OS ensure protection between two user processes?  
OS allocates seperate address spaces for each process. Processes cant access each other's address spaces 

5. How does an OS ensure its own protection as well as that of the other system resources?  
It uses a dual prividledge system. To change anything OS related you need superuser permissions, to change other processes you need user level permissions

6. Sort the following levels of storage hierarchy by their access time and size.  
(a) Registers, (b) cache, (c) disk  
a -> b -> c (c is slowest)

7. What factors restrict the number of architected registers?  
Distance from CPU, size
8. What factors restrict the size of the cache? Why have multiple levels of caches?  
The bigger it is, the harder it is to find what you are looking for which makes it slower. To have multiple layers of storage

9. Explain if true or false:  
(a) The architecture registers are managed by the hardware.  
True, registers managed by the CPU
(b) The user can directly manage the disk.  
False, requires OS

10. The OS provides a virtualization. Explain.  
No it is a layer between OS and hardware
# Chapter 2  
1. Name a few OS services to help the user.  
CLI, GUI, API, logging

2. Name a few OS services for efficient system operation.  
Resource allocation, security, 

3. What are system calls? How do they differ from other function calls?  
System calls are kernel mode services that do specific tasks like aloccating memory. They are different because they are not part of the C language but instead are imported from the C std libraries. They also require kernel mode permissions 

4. What is an API? How is an API call different from a system call? Which one would you  prefer to use in your high-level program?  
API is an interface to the sys calls that makes it more portable in case the system is different. API is preferred

5. What are the limitations of passing function arguments in registers? How are these limitations overcome?  
There are only so many registers, it is overcame by passing it through the top of the stack

6. What do the following system tools do: (a) compiler, (b) assembler, (c) linker, and (d)  loader?
Compiler takes C to assembly, assembly takes assembly to obj files. Linker combines multiple object files and then converts to binary. Loader puts the program into memory

# Chapter 3  
1. Define process. How is a process different from a program?  
Process is a program that is currently being executed or is loaded into memory ready to be executed while program is a set of instructions that are not being executed

2. Illustrate (draw) the process address space. What component of your high-level C program goes into each section of the process address space?  
stack and heap are dynamically allocated (grows) and the stack holds functions and vars. Heap holds other vars
Size of data is static and holds global vars. Text is below that and holds the program code itself

3. The process state diagram below shows all the possible process states, but only two process  transitions. Complete the figure by illustrating how the process changes states on the following events: (a) interrupt, (b) scheduler dispatch, (c) I/O or event completion, and (d)  I/O or event wait.
![[Pasted image 20241005203258.png]]
interrupt (a) goes from running to ready, schedular dispatch goes from ready to running, io event completion (c) goes from waiting to running and wait goes from running to waiting

4. Which of the following typical kinds of CPU instructions should be privileged? Explain your reasoning briefly.  
(a) Set value for hardware timer  
Yes, modifying something OS level
(b) Read hardware clock  
No, just reading
(c) Set a value in the shared memory region.  
No
(d) Switch from user to OS execution mode  
yes
(e) Turn off hardware interrupts.  
yes

5. Describe what a kernel must do to switch context between two processes. What are the  benefits and drawbacks of context switching for an OS?  
The benefits is that it makes the computer able to do more things at a time like go from one task to another and then comeback to the first task all while remembering where it was at. It does make it slower though.  In order to switch it has to save the current PCB onto memory and load the PCB from memory onto the cpu

6. What is the purpose of the fork and exec system calls?  
Fork is to create a new process and exec is to run a command like ls that will override the rest of the program

7. Using appropriate system calls, have the program below create a new process, and execute   the program /bin/ps. It is not important to get all the function arguments and their   positions correct.  
```c
int main()  
{  
	pid_t pid;  
	/*create a new process*/  
	pid = *fork();*
	if(pid == 0){  
	}  
	else if(pid < 0){  
	}  
	else if (pid > 0){  
		*execl(BINPS);*
	}  
}  
```


8. Compare the shared memory and message passing IPC models as regards: (a) characteristics  (b) efficiency (c) ease of use. Explain  
Shared memory: 
Allocated memory in with protections overridden so it can be read/write to by other processes. Very fast and convinenient but have to be careful because you can overrwite what is already there
Message passing:
Like a mailbox in the kernel space where messages are put in a fifo Queue. Great for multiple producers/consumers but slower because need to make syscalls and no conflicts because message added to the end. Instead, you have to make sure the queue doesn't fill up

9. Consider the program below:  
```c
int main()  
{  
	pid_t pid;  
	int shared_num = 0;  
	/* create a pipe */  
	int fd[2];
	pipe(fd);
	/* create a process */  
	pid = fork();  
	if(pid > 0){  
		/* parent process */  
		/* write numbers to the shared variable*/  
		int i;  
		for(i=0 ; i<10000 ; i++)  
		shared_num += 1;  
		wait();  
		fprintf(stdout, "Shared variable = %d\n", shared_num);  
	}  
	else{  
	/* child process */  
	int i;  
	for(i=0 ; i<10000 ; i++)  
	shared_num -= 1;  
	}  
}  
```
Use pipes to synchronize access to the shared variable so that the final result printed in the parent process is 0. (You need to first understand why the answer printed may be different  from 0).  

10. Compare shared memory based IPC with message passing based IPC.  
Shared memory is faster but more complex because you can override

11. What are the advantages and limitations of pipes IPC? How do some other IPC mechanisms  address these drawbacks?  
The issue with pipes is that they are one directional and only from one process to another. Others address this by making them bidirectional (shared memory), allowing more than one consumer/producer (message parsing) and persisting after file ends and allowing non parent-child to access it (named pipes)

12. List and explain in a line the system calls used to setup a shared memory segment.  
shmget: Makes a shared memory section
shmat: attaches to shared memory segment
shmdt: Detaches
shmtcl: Control operations

13. Explain the (a) server and (b) client side sequence of calls to set a socket between a client  and a server.  
server: socket(), bind() listen() to start and then accept()
user: connect()
# Chapter 4  
1. Draw the process address space of a process with two threads. Indicate the stack, heap,  text, and data space of the two threads. How is the process address space different if we  had two processes instead of two threads?  
So with two threads, everything is shared except the stack which are two seperate. 
With processes, everything is different

2. What the (a) advantages and (b) disadvantages of threads over processes?  
advantages: Faster, simpler to share data
Disad: less isolation, crashing/exec stops all threads

3. Differentiate between (a) user-level and (b) kernel-level threads. What are their advantages and disadvantages over each other?  
User level threads are fast and flexible because they dont need a syscall but if they get blocked, it causes all threads to be blocked
Kernel level threads: are slower but user more widely because they are better for multicore and don't stop everything if the thread is blocked

4. Name and illustrate the three multithreading models.  
One- to- one: Used in linux. each user level thread is connected to one kernel level thread. This way if one thread is blocked on the user level, every other user level thread is also fine. Also makes multicore better
Many to one: Many user level threads mapped to a single kernel level thread. BAD because if one user level thread is blocked then they are all blocked
Many to many: many user level threads connected to many server level threads. This is more fore servers because it reduces the spinup time for threads.

5. What is the main disadvantage of the many-to-one multithreading model?  
Bad for multicore programs because one thread freezing up will cause all of them to

6. What system call is used to create threads in Linux?  
pthread_attr_init
7. Write a simple program that contains two global variables num1 and num2. The program  then: (a) creates two threads, (b) Thread-1 performs num1+num2 and returns the result,  (c) Thread-2 performs num1-num2 and returns the result, (d) Main thread displays the two  returned results and exits.  

8. How do system calls fork and exec operate if invoked from multi-threaded programs? (You  may use Linux as an example.)  
forks clone only the main thread and exec overrides all threads

9. Explain the two mechanisms of thread cancellation.  

10. How are synchronous and asynchronous signals handled in a multi-threaded program?
