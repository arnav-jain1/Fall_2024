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
	Server has to use socket() listen() and recieve() (and bind)
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
```# Process overview/recap
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

# EXAM 1 END SLIDE 21
## Thread termination
Killing a thread before it is done
Asynchronous cancellation:
	Cancels the target thread immediately 
	Allocated resources may or may not be freed
	Shared data may be ill-defined
Deffered cancellation: 
	Target thread elims itself
	Easy to make orderly
	Failing to check cancellation status may cause issues
Linux supports both (pthread_cancel)

## Signal handeling
signals used to notify a process about an event
Signal handler processes these signals
	OS can either handle it or deliver it to the proper process
Types:
	Asynchronous: Generated by an event outside the process
	Synchronous: Generated by event inside the process
Options on where to deliver
	Deliver to thread where signal applies
	Deliver to every thread
	Deliver to certain thread
	Assign a specific thread for all signals

## Implicit threading
Writing correct multithreading is hard and can slow down performance/latency
Compilers and runtime libraries can manage threads semiautomatically
	Thread pools
	OpenMP

## Thread pools
Fixes the issue of continously creating and destroying threads which is slow and/or trying to create a thread you have no more space/processing power
Thread pools create a list of threads in a pool where they wait for work, at most the number of processors
This makes it faster to service a request because you don't have to make a new thread every time and makes it so you can't exceed a certain number of threads

## OpenMP
Compiler directives and API for C, C++, Fortran
Supports parallel programming 
User defines a parallel region using scope and OMP creates as many threads as possible

## Kernel threads linux
Linux refers to them as tasks
Creation done through clone() syscall and allows child to share address space of parent task (process)
![[Pasted image 20241011132212.png]]

# Multicore processors
Multiple processing cores on a chip
Speed is reaching a limit because of power/heat, limits to freq scaling, transistor scaling is still viable
	Transistors are leaky (lose power) which is why they need so much power which generates heat
More cores on one computer > more computers because same chip communication is faster and memory sharing is easier and faster
![[Pasted image 20241011132430.png]]
Challenges:
	Dividing activities and balancing workload
	Splitting data
	Data dependencies between currently running tasks
	TEsting and debugging# Ch 5 sync
## Why important
One issue with parallel programs are race conditions
2 major issues
	Memory problems
	Race conditions

Why synchronization?
	Processors cooperate with each other 
		Producer and consumer model
		Concurrent execution (multicore)
	Processes share data
		When we share data, it may stop being correct
	Why is correctness in danger?
		Context switches can happen at any given time (interleaving, leaving mid process)
		Processes run concurrently
		Different orders of updating the data may lead to different values (not functional)
	Process synchronization
		Done to coordinate updates to shared data (multiple processes writing at the data at the same time)
		No issue if there are no writers

![[Pasted image 20241011141516.png]]
Bounded buffer problem: Writes cannot happen if buffer is full
2 counters:
	*In* is the index where we put in the data and out is where we pull *out* the data. 
	Counter has number of items in the buffer, shared
If counter++ is atomic then there is no issue but if it is not then 
If there are multiple producers then in is also critical
![[Pasted image 20241011142306.png]]
Atomic instruction is the most basic unit that WILL complete and can't be interjected. So R1 = load(counter) WILL happen but you can interject between instructions
If counter++ is atomic (a single instruction) then this issue will not happen

Race conditions:
	Several processes manipulate (write, etc) data at the same time and final value depends on the order
	*Race* between multiple processes

The critical region that updates shared data
Updating data at the same time is dangerous
## Solution
Simple Solution (in theory) is to only allow **ONE** process to enter and be in the critical section at a time
	Hard in practice
	Protocol:
		Request permission to enter critical section
		Indicate exit from section
		Only one process at a time
Solution should include:
	Mutual execlusion: Only one process at a time
	Process: Selection to enter should be fair and decision not postponed forever
	Bounded waiting: Fixed amount for how long the wait will be before access to critical section (timed and/or line ie at MOST 5 min or at MOST 3rd in line)
	Also guarding against *deadlocks*

# Small aside, Preemptive vs Nonpreemptive kernel
OS has many processes and user is using many parts of the OS that is shared (like the file and memory management table)
premption: Process time slice expires so going back to ready

Preemptive kernel: Process can be preempted (moved back to ready when time is up) while executing kernel mode program
	Can run into issues when it is moved to ready before done with (shared data)
		IE updating file in file table, if moved before done, it is a shared data so it might not be what you expect
		Good for real-time programming because more responsive
	Linux
Non-preemptive kernel: OS code can run indefintely 
	Interrupts disabled 
	Became less popular with multicore processors because harder to do and not effecient 


# Sols to Critical section problem
## Peterson's sol
Software based
Supports only 2 processes (can be extended but looks ugly)
2 shared vars:
	Turn: Whose turn it is to enter the critical section
	flag: indicates whether it is ready to enter 
![[Pasted image 20241011144617.png]]
Flag indicates whether it is ready to enter the section
The while loop blocks
Not guarenteed which one goes first

Meets all requirements:
	P0 and p1 never together at the same time
	P1 does not have to wait if p0 does not want to enter
	process waits at most one turn
But only 2 processes (others are very messy and bad)
Assumes load and store are atomic
Also assume that the memory accesses that are unordered
Might be less effecient than hardware approaches (especially if >2 processes)
Another issue is that one process can become blocked (execute while loop while not doing anything)

```
P_0: flag = [1,0]
P_0: turn = 1
P_0: while (false && true)
P_0: Critical section

P_0: flag[1,0]
P_1: flag[1,1]
P_0: turn = 1
P_1: turn = 0
```

## Locks
General solution
	Critical sections are locked 
	Processes lock on entry and then unlock on exit
## Hardware support
For 1 core:
	Concurrent processes cannot overlap, only interleave
	Process runs until sys call/interrupt
To fix an interrupt happening while in critical section just disable and reenable but it disabling it can create infinite loops

Multiprocessors: Processors are doing stuff independanly 

Disassabling interrupts is too ineffecient and not scalable for OS
Hardware support with atomic instructions
	Atomic test and set, swap, compare and swap
	Treated as one step, cannot be interleaved
![[Pasted image 20241018143842.png]]
target is saved, then changed to true, then the old target is returned
all in one step

![[Pasted image 20241018144047.png]]
If mutex is false, it is set to true and will continue through the while loop since it returns false. 
If it is true, then it will continue

3 desirables:
	Mutual exclusion: maintained because only at once
	Progress is mantained because process gets access right away if critical section is free
	Bounded waiting (fairness): Not guarenteed because one can be in critical section for any amount of time



Getting fairness
![[Pasted image 20241021141241.png]]
Assumption: 
	processes numbered 0..n
	waiting is shared but one spot for each process
Initially, the lock is false

the while loops keep going until unlocked
The j part checks (sequencially) if the other processes are waiting and then it will hand the lock over to another process (only unlocks if none are waiting)
This gives fairness
<mark style="background: #FF5582A6;">not entirely sure why</mark>


## Semaphores
Higher level solution than ISA instructions (provided by OS)
Similar to locks but with different semantics
Simple def:
	int value accessed in init, wait, and signal
	All are atomic
Binary semaphores: 0,1 for mutex
Counting semaphore: any int value with access to some finite value

![[Pasted image 20241021142700.png]]
***SIGNAL INC, WAIT DEC***
Similar to before
S should always be 1 or 0
Only one process waiting at a time, it will wait until the other process gets to the signal

Desirables
	Mutual exclusion: maintained because only at once
	Progress is mantained because process gets access right away if critical section is free
	Bounded waiting (fairness): Not guarenteed because one can be in critical section for any amount of time

Spinlocks: Process spins forever
Issues with the previous ones is that the processors are waiting and not doing work, they are spinning and the wait could be forever
	Note: Multiprocessors still use busy waiting


### Semaphore w no busy waiting !!
Associate waiting queue with each semaphore 
```c
typedef struct {
	int value;
	struct process *list
} semaphore;
```
![[Pasted image 20241023141941.png]]
wait will suspend the process entirely and signal will bring it from ready to execution
These 2 still need atomicity
	One way is to dissable interrupts
	Another is to have spinlocks around wait and signal
Spinning not entirely gone but shifted (and reduced)


### Deadlock/Starvation
2+ processes waiting for each other to do something that only the other can do
Essentially both are just waiting for each other
![[Pasted image 20241023142738.png]]
If both S and Q are 1, then P0 is waiting for P1 (Q to become 0) to finish and and p1 is waiting for p0 (S to become 0) to finish.


Starvation is when process is waiting indefinitely (will never be removed from semaphore queue)
	caused by scheduling
Priority inversion: Lower-priority process holds blocks a higher priority process from running
	Example, 3 processes L < M < H
	Let L be producer and H be consumer
	Since M has higher priority, M preempts L this causes L to be after M and since H needs to wait for L, H has to wait for L and M
	sols: 2 priorities max, give the producer that is lower priority higher priority

#### Bounded buffer problem
Set of resource buffers shared by producer and consumer
Producer waits when buffer full and consumer waits when empty
We want consumer to see each item EXACTLY one time
SOlution (3 semaphores):
	Mutex semaphore that allows exclusive access
	Empty (counting semaphore) which counts empty buffers (initialize to num buff)
	Full: same as empty but for full (initialize to 0)

![[Pasted image 20241023144146.png]]

#### Readers - Writers problem
Some threads only read (not consumers, only read dont remove), others only write
Readers can go at same time but writers can't go concurrently
Want to make sure they don't overwrite each other and readers dont read while being written
2 semaphores:
	mutex: ensure mutual exclusion for readcount, initialized as 1 (readers block each other from updating a counter)
	wrt: ensure mutual exclusion for writers and between writers and readers

Signal always increments semaphore value
## Drawbacks
Essentially just shared global vars
Very low level constructs, the connection between the semaphore and the data controlled by semaphores is non existent 
Hard to use it 

# Monitor
Programming language construct that controls access to data
	Sychronization code added by compiler but enforced at runtime
Abstract data type that has:
	Shared data structure
	code that operate on shared data struct
	Sync between concurrent code
Guarentees that only the monitor code can update the data


![[Pasted image 20241025140953.png]]
Only one thread can execute at a time
Other threads wait
When one active thread exits, another enters

For example in Java, the synchronized keyword indicates that only one thread can access it at a time

The issue is when having to wait
If consumer has to wait for producer and gets there first, then it is bad
![[Pasted image 20241025142253.png]]
To fix this then there is condition values that when the condition is met, the lock is reaquired and the critical section is reentered. 

Condition vars have 2 vars:
	wait(), if cond not met, thread moved to wait set, gives up lock
	signal(), wakesup waiting thread (1), if no process suspended, then does nothing
		Some programs have broadcast to send signal to all threads
Another problem is that if there is an active thread that invokes a signal, what happens to the red suspended thread? It can't go in because it is locked already
2 ways to handle:
	Hoare monitors: signal switches from caller to waiting thread, waiter will have its condition met when it starts because the signal immediately swaps
	Mesa monitors: waiter set to ready, signaler continues and the waiter is sent to the ready queue. Issue is condition might not be met when waiter starts
Signaler should immediately leave and the waiter should start

![[Pasted image 20241025143005.png]]
Producer will add to buffer and then send the signal, if it is full then it waits
Consumer will get the resource and then send the signal back, if it is empty then wait

The conditions are not booleans, they are the signals
Conditions $\ne$ semaphore
	In semaphore wait blocks and signals increases global var count


Sleep vs spin (wait)
Spin: 
	Occupies CPU, slows other threads
Sleep:
	issue wait and sleep, signals to sleeping thread to start and then wakeuping up thread involves lots of context switches

Spin can be faster if critical section is small
Spin waiting used on multiprocess systems, small critical sections, thread holding the lock is running

# 0 is lock# Basic concepts
Multiprogramming
	Alternate between CPU and IO bursts
		IO burst: Process does IO and not use CPU (free/idle)
		CPU burst: Process does CPU and not use IO
	Can schedule another process during IO burst (max the util)
CPU bound
	Speed bounded to CPU speed (better cpu = faster)
	Most time is doing CPU, long CPU bursts
IO bound
	Spend most time doing IO
	Does not depend on CPU speed
	Few short CPU bursts

# Scheduler
Picks the next process to run
	Part of the OS *dispatcher*
	Selects processes from memory that are ready to execute
	Utilizes a strategy
Happens when process switches from
	1. running to waiting (when IO)
	2. running to ready (time slice is up)
	3. waiting/new to ready (check if the priority is higher than currently running)
	4. terminates
All 1 and 4 is nonpreemptive (process voluntarily finishes), others are preemptive (Involuntarily)

Nonpremptive:
	Process voluntarily is done with CPU
	Easy, no special hardware (no interrupts, timers)
	Bad response time for interactive and real-time systems
Prememptive (better):
	OS forces process to leave  the CPU (higher priority or time slice runs out)
	Special hardware like timer
	Needs to maintain consistency 
	Complicated but preferred
	Favored by OS


## Dispatcher
Scheduler is part of dispatcher
Goals:
	Get new process from scheduelr
	Context switch (remove current process)
	Give CPU new process
	Jump to right spot in new program to restart 
Time taken to do this is called dispatch latency


## Scheduling queues
Job queue: All processes 
	Long term scheduler pareses them and brings them into memory 
	All processes are in job queue
Ready queue: Processes in memory
	Ready and waiting for execution
	Scheduled by short-term scheduler
Device/IO queue: waits for a device
	Process can be blocked for same device 
	IO completion moves back to ready queue

![[Pasted image 20241028141703.png]]
Ready queue is the queue that is waiting to be processed
Can escape the process if:
	IO request, then goes to IO queue and once doen then goes to ready
	Time slice: Put back in ready
	Fork: When child done, goes back to ready
	Interrupt wait: When interrupt occurs, goes back to ready

## Metrics
How to measure how good each scheduling algo is 
Maximize:
	CPU Util: % of time CPU is busy (includes busy waiting)
	Throughput: Number of processes that finish per time unit
Minimize:
	Turn around time: How long it takes to finish a process once submitted (finish-submit)
	Waiting time: How long a process is waiting in the ready queue (time it is not running and not done)
	Response time: Amount of time it takes from request submitted until resposne
Also want to be fair to all processes and users


Evaluation criteria:
	Give importance (or weight) for each metric
Deterministic modeling (what we use):
	Take a workload (group of processes) and gets the perf results for each algorithm
	Simple and fast w exact numbers
	Difficult to generalize (what tradeoffs are good/bad may vary)
	Shows algo trends

Workload model:
	![[Pasted image 20241028142738.png]]
	Shows list of processes and information
	Process 1 arrived at 0 and did a CPU burst for 8s
	P2 arrived at 1 and burst for 4
	P3 arrive at 1 (but exec after because P2 is first) and burst 10
	P4 arrived at 6 and burst for 2

Gantt chart:
	For batch scheduling algorithm (non premp)
	![[Pasted image 20241028143022.png]]
	P1 starts, exec for 8 then P2. P2 exec for 4 then P3 for 10 then P4 for 2

Same workload model will have different gantt charts

![[Pasted image 20241028143342.png]]
For process A:
	Submitted at 0
	Response time is 0
	Turnaround time = 9
	Wait time is 2+2+1 = 5 (time it is not running and not done)
For process B:
	Submitted at time 0
	Response time is 1
	Turnaround time (submission to completion) = 5
	Wait time = 3


Queuing model:
	Theoretical model
	Represented by a bunch of equations 
	Depends on a bunch of assumptions
Simulations:
	Most common, best
	Simulate a coded scheduling algo based on a bunch of data
	Time and space intensive
	Not always deterministic (small variations, systems are complex)
	Overhead of algorthim (how long it takes to decide) is considered

![[Pasted image 20241028143840.png]]
Get trace of a real system (list of processes ran inlcuding cpu burst, times, etc) and then run the trace


# Algos
## First come first served
The first request is the first served (FIFO)
No preemption 
	No time slice
Advantages:
	Easy to write
Disadvantages:
	waiting time might be long
	Doesn't balance IO bound and CPU bound processes
	Convoy effect: Long process before short process so the average wait time goes up
	Not usable for time sharing systems
![[Pasted image 20241030140814.png]]
P1 will run from 0 to 24, then P2 from 24 to 27 and then p3 from 27 to 30
Waiting time $WT(P_{1})$ for $P_{1}$ = completion time - submission time - time burst (running time)
	= 24 - 0 - 24 = 0
$WT(P_{2})$ = 27 - 0 - 3 = 24
 $WT(P_{3})$= 30 - 0 - 3 = 27
Average wait time = (0+24 + 27)/3 = 17

Turnaround time for $P_{1}$ = Completion time - submission time
	= 24 - 0 = 24
$TT(P_{2}) = 27 - 0 = 27$
$TT(P_{3}) = 30 - 0 = 30$
$Av(TT) = \frac{24+27+30}{3}=27$



If the order is $P_{2}, P_{3}, P_{1}$
This improves everything significantly
![[Pasted image 20241030141718.png]]
Issue is that the average is very variational, and no preemption (bad for interaction and real time systems)
But the scheduling is constant time, it is fast and scheduling the job is fast and always the same

## Shortest job First (SJF)
Order each of the processes based on how long their burst is (how long it takes to run)
The shortest job runs first
Advantages
	SFJ is a greedy algorithm and optimal because the wait time will always be the shortest possible 
	Good benchmark
Disadvantages:
	Difficult to know length of the next CPU request (user might not even know) so it is difficult to implement (unrealisitc)
	Leads to process starvation if there is a process with a long CPU burst and there are always shorter processes 

![[Pasted image 20241030142407.png]]
P4 (3) runs from 0 to 3 -> P1 (6) runs from 3 to 9 -> P3 (7) runs from 9 to 16 -> P2  (8) runs from 16 -> 28

$WT(P_{3}) =$ 16 - 0 - 7 = 9
Av wait time = 7
We know that 7 is the BEST wait time

Since we don't know how long it will take, we need a way to predict it
### Estimating length of next CPU burst
Done by looking at CPU burst of how long the process took in the path
	Calc as exponential av
$t_{n}$ = actual length of nth CPU burst
$\tau_{n+1}$  = predicted value of the next burst, random at the start for n=1
$\alpha, 0 \le \alpha \le 1$ 
$\tau_{n+1}= \alpha t_{n} + (1-\alpha)_{\tau_{n}}$     
If $\alpha = 0$, then history does not count, random guess
If $\alpha=1$ then only the last CPU burst counts

$\tau_{n+1} = \alpha t_{n} + (1-\alpha)(\alpha t_{n-1} + (1-\alpha)(...))$   so the previous generations are weighted less and less because the $\alpha$ gets squared for the gen before the prev, cubed for the one behind
![[Pasted image 20241030143957.png]]
Inital guess was 10, actual was 6
so $\tau_{2} =.5*6 + .5 * 10=3+5 = 8$
Actual $t_{2}=4$ 
so $\tau_{3}= .5 * 4 + .5 * 8 = 6$
...

One of many models


Adding preemption to SJF is when a new shorter process comes in

### Preemptive SJF
Previous one is not preemptive, once scheduled, it will run for full time slice
With this one, if a new shorter process is scheduled, it might preempt 
For review, prememptive if:
	New process created
	Time slice expired
	IO done 
	Higher priority process

$WT(P_{1})$ = 12 - 0 - 8 = 4
$WT(P_{2}) =$ 5 -1 - 4 = 0
$WT(P_{3})$ = 26 - 2 - 9 = 15
$WT(P_{4})$ = 10 - 3 - 5 = 2
$WT(Av)$ = 6.5


### Priority Scheduling
Every process has a priority and CPU does highest priority 
Externally assigned (someone else specifies, like I specify when I put it in)
Internally assigned (intrinsic to the algo like SJF and FIFO)
Preemptive or non preemptive 

Lower number = higher priority 

Advantages: Priorities made as general as needed
Disadvantage: Lower priority might never exec
Aging: Used to prevent starvation, Increase the priority of the process with time

### Round Robin scheduling
Each process given a fixed time quantum and then preempted and then next process runs
	Ran in FCFS 
	Allocate CPU to first job in queue until its time slice runs out and then it is put at the back of the queue
Preemptive by def (Because it gets preemptive if done with time slice)
	If you increase quantum to inf, becomes FCFS (essentially FCFS with preemption)
Advantages:
	Simple, no starvation
Disadvantages
	Likely a large overhead because context switch
	Reducing the quantum will mean more overhead because of context switches
	IO bound process will run slower on memory loaded system

Essentially the multitasking OS

Performance depends on length of time quantum
	Large time quantum = FCFS like behavior 
	small time quantum = large context switch overhead
Time quantum is usually from 10-100ms
Context switch time is usually 10microseconds
RR has bigger wait time, but better response time for interactive systems
Turnaround time depends on size of time quantum
![[Pasted image 20241101143453.png]]

### Lottery scheduling
Address fairness problem
Process is given some number of tickets based on priority or other properties
OS knows how many tickets have been allocated and one is chosen as random. The process with that ticket is ran for its *time quantum* (not whole time)
	To replicate SJF, shorter jobs get more tickets

To avoid starvation, each process gets at least one ticket

![[Pasted image 20241101144009.png]]
Example 1: There is 1 short job and 1 long job so the ticket diff is 10/1 so 10/11 for short job and 1/11 for long job
Example 2: 0 sj, 1 lj. 2 tix total. Both long jobs have 1 ticket each so 50% each
Example 3: Same but 20 tix total
Example 4: 10 sj, 1lj so 101 tickets total. Each short job has 10/101 chance to run while each long job has 1/101
Example 5: 1 sj and 10lj so 20 tix total. The short job is 10/20 while each long job is 1/20

### Multilevel queue
Multiple queues: foreground and background with them having different scheduling algorithms
	Foreground queue is for interactive
	Background queue is for FCFS
2 scheduling algorithms, one to pick the algorithm and then the algorithm runs
Queues themselves can have priorities and the processes within the queue can have priority
![[Pasted image 20241104140426.png]]

Scheduling done between the queues:
	Fixed priority scheduling: Select the higher priority process in the higher priority queue
		Starvation possible
	Time slice: Each queue gets certain amount of time on CPU to schedule its processes
		aka 80% foreground in RR and 20% background in FCFS


### Multilevel feedback queue scheduling
Processes/jobs can move between the queues based on its features
Example:
	Multiple queues with different priorities
	Round robin scheduling at each priority level
	Highest priority queue first -> next highest -> etc
	Jobs start in highest queue, if time slice expires, move it down, if it does not, then move it up
	This allows the shortest bursts to finish before the highest 
![[Pasted image 20241104141317.png]]
A will run for 1 time and then priority lowered to 1
Then B will run for 1 and then lowered to 1, same with C
Since there are no more jobs in P0, we move to p1
![[Pasted image 20241104141408.png]]
A now runs for 1 and is done (lets say it waits for IO)
Then B runs and finishes
![[Pasted image 20241104141541.png]]
Then lets say A comes back with time slice of 1, since A did not use its last time slice, it goes back to P0 and then runs first for 1 time unit.
Then C runs for 2 out of 3 but then gets moved to P2. 
C then runs in P2 where it finishes (will be moved back to P1 if returns)
![[Pasted image 20241104142007.png]] time = 10

### Solaris dispatch
![[Pasted image 20241104142217.png]]
59 is highest, 0 is lowest. 
Time quantum expired is where it is placed if time quantum is done
return from sleep is where it is placed after being done

Approximating Shortest remaining time first (SRTF) because the CPU bound jobs are lower priority while IO bound will be higher
Unfair for long running jobs
	Add aging: Longer the waiting process, it gets moved up

### Thread scheduling
Contention scope: For user level threads (when mapped to one+ kernel threads)
	PTHREAD_SCOPE_Process: Contend for time on kernel thread between user threads
	PTHREAD_SCOPE_system: Assigned to kernel thread, contends with other kernel threads
Lets say 4 user threads to 1 kernel thread and 4 user thread to 4 kernel thread
So system sees 5 threads total. Each kernel gets 20% (system contention) and each user gets 25% on the kernel (process contention)

When can also tell the thread to inherent the scheduling algo from parent thread or explicitly specify using attribute obj that we have
	SCHED_RR (Round robin), SCHED_FIFO (fifo), and SCHED_OTHER (other)
	Can also set param using schedparam

	Memory is limited, usage keeps going up
# Background info
CPU can only directly access memory or registers
	Instructions and data must be in memory to be accessed

Memory is just a large array of bytes
Accessing it is slow (compared to register/cache)
	Cache faster because smaller and closer (on CPU)
	Memory takes more clock cycles to access and you have to take a BUS
Memory needs to be protected 


# Old model
OS is generally placed in low memory address (for interrupt vector tables)
![[Pasted image 20241111140957.png]]
Each process has its own address space assigned to it with a base and limit (stored in registers)
If process requests address outside the range, then seg fault


# Address binding
Instructions and data is all binded (stored) in memory addresses 
There are 3 possible times where addresses can be bounded:
	1. Compile time: If the location of the memory is known at compile time, absolute code can be generated (will have to recompile if address changes)
		Can be done when using obj files
		Necessary that the generated address places are placed at the right places
		location should be free when running
	2. load time: Compiler has to generate relocatable binary if the memory location is unknown at compile time (code placable anywhere in memory)
	3. Execution time: Binding delayed until runtime to allow program to move while being ran
		Need hardware support but most flexible 
		Most common way but has overhead
![[Pasted image 20241111141344.png]]

When you compile normally, the instructions are symbolic with no actual values, instead relative values

Logical address space (generated by CPU) is bound to a physical address space
	Changed by the OS
Logical == physical in compile-time and load-time
Different in execution time


## MMU, memory management unit
Hardware that maps a virtual address to a physical address
Simple scheme:
	Start process's logical address at 0
	Max address stored in limit register
	Base memory address stored in relocation register
	The physical address is added to the relocation register 
![[Pasted image 20241111143404.png]]
Range for virtual: 0 to limit register
Range for physical: relocation register to relocation register + limit register
Done in hardware because faster

In compiletime/load time, does not exist 


## Dynamic loading
Static loading:
	entire program is in memory before program starts
	Program size is limited
Dynmaic loading:
	Routine not loaded until called
	Better util, unused routine never loaded (like with if else blocks)
	Useful for edge cases (used rarely but need a lot of code)
	No support from OS, implemented through program design

## Dynamic Linking
Static Linking:
	Links all files at compile time
	Compiles libraries and into a binary and links it as compiles it at compile time
	Increases binary size and multiple copies of the library may exist at a time
Dynamic Linking:
	Linking postponed until exec time
	Library stored in memory, a small piece of code (stub) used to locate the location of the routine (pointer to the function pretty much)
	Only one copy of the library at a time
	*Shared* library

## Swapping
Swapping is when a process is moved from the memory to a *backing store (disk)* and then swapping another process to the freed space
	Pages get swapped in and out
Useful for when memory runs out
Since it is stored on disk and disk is slow, it is very costly
	Leads to memory thrashing if done a lot

![[Pasted image 20241115130500.png]]


# Contiguous memory allocation
Effecient allocation of memory is important because it is limited and contains the OS/active processes
Divided into 2 partitions: The OS in the lower region with intrurrupt vec table and user processes in higher memory

Contiguous memory allocation is when each process is in a single continuous memory block
	Allows for multiple processes to be there at the same time
	Processes protected from each other


## Hardware support for prot
Limit registers used to make sure the address is less than the limit, if it is, then the relocation address is added
	Protects processes from other processes and protects OS
![[Pasted image 20241115131425.png]]
Since relocation register is the lower bound, we dont have to check the lower bound


Multiple parition method:
	Holes are blocks of available memory and are scattered through address space
	When a process arrives, it is allocated memory in the hole
	The OS remembers (saves) info about the hole and allocated partitions
	![[Pasted image 20241115131637.png]]

The issue with this, is how does the OS allocate memory for a new process of size n
	**First-fit scheme**: Allocate first hole that is big enough, fastest
	**Best-fit**: Allocate the smallest hole that is big enough (searches the list unless ordered), produces smallest leftover hole
	**Worst-fit**: Same thing but for biggest hole. Produces biggest leftover hole (not effecient)



### Fragmentation
External Fragmentation: There is enough memory to run the process but it isn't continuous
Solutions:
	Compaction: Move blocks to place the holes together, only possible if dynamic
	Allow for non continuous allocation of address space
Internal Fragmentation: 
	Happens with blocks of memory where the size of the block is slightly larger than needed so that the hole isn't super small
	Leads to memory loss

# Paging
Solution to external fragmenting and compaction
Commonly used
Depend on HW support
Allows for noncontinuous address space blocks

Scheme:
	Divide the *physical memory* into fixed size blocks (frames), usually power of 2
		Refer to the actual address
	Divide the *logical address* into blocks of the same size (pages)
	OS tracks all of the frames
	A program that is **n** pages big, needs n free frames
	Page table translates pages into frames
Each of the pages matches up with a frame in the phsyical address space

The size of the page == size of frame (specified by the hardware)
The page size determines the amount of bits needed to specify the offset

The offset is the number of data a page can hold

So if our page size is n bytes, the offset will be $log_{2} n$  

So lets say the page size is 32, the offset will be 5 
Now lets say the total address bits is 12 (arb)
then the page number is now 12 - 5 (offset) = 7 so then there are 2^7 pages

Lets instead say address bits is 7, so then 7-5=2 so 4 pages
![[Pasted image 20241115142238.png]]

![[Pasted image 20241115142721.png]]
So lets say finding logical address 1 to physical
The page # is 00 (binary) and offset is 01 so the physical would be 10101=21

Lets look at 5, it is on page 1 so 01 and offset is 01 so 0101. The physical would be 6 (page table) so 110 and offset stays same so 01 so 11001 which will become 25

For 10, 1010 (write in binary is step 1) 10 regers to page 2 which refers to page 1 so 001 and then the offset carries over which is 10 so then it becomes 00110 which is 6

For 15, 1111 so offset is 11 and page num is 11 (3) which corresponds to 2 which is 10. Then bring the offset back so 01011 which is 11


### issues with page tables
This still leaves internal fragmentation 
Page size
	Small page size reduces internal fragmentation but increases size of page table
	Larger page size reduces size of page table
	Larger transfers is more effecient 
Each memory load/store operation goes through the page table
	Page table has to be fast (hardware registers)
	Only possible for small tables, 256 entries max
Each process can have its own page table
	Page table context switching should be fast
Most computers have large page tables with pointers to the page table (in memory)


### Implementation
Page table base register (PTBR): Points to page table 
	Only part affected with a context switch
Every load/store requires 2 memory accesses
	One for the page table and another for data/instruction
Translation lookaside buffer (TLB):
	Cache to hold popular page table entries
	Should be fast, uses associative cache
![[Pasted image 20241115144819.png]]
Power hungry part but very important

#### Issues
Small page size reduces internal fragmentation (less wasted space) but increases page table
Large size does the opposite

Each memory and stores goes through the page table so it has to be fast (registers) which means limited to 256 entries
Each process has its own page table
Most Computers allow large page tables (kept in memory) where register points to page table address



Page-table Base register (PTBR) points to page table (only this register is affected when context switching)
	Every load/store uses 2 memory access (1 for the page table and another for the actual data)
Translation look-aside buffer (TLB) Cache that holds the most popular page table entries
	Needs to be very fast (associative cache)
	Since it an associative cache, it can search for everything at the same time (takes power)
		But if it is too large then it takes too long and more power

### Memory protection
<mark style="background: #FF5582A6;">If CPU has 4 address bits, then the logical address space is 16 bytes, page size is 4 bytes</mark>
	If there is are 2 processes P1 and P2 that are using 16 and 6 bytes respectively, both will have a page table that is 4 pages long
	The unused one are set to invalid

Associate protection bit to each frame
	Valid is to show the address space maps to some frame which is good and allowed
	Invalid shows that there is nothing is there and that it is not mapped to some frame (which is why you get a seg fault)

Another example:
	14 bit addy space where the program goes from 0-10468
	Page size of 2kb 
		So 8 pages total
	Pages 6 and 7 exist but are invalid
	![[Pasted image 20241120143646.png]]

### Shared pages
Allows the sharing of code
	The code is set to read only and is shared
	Each process has its own area as usual
	reduces memory redundency
Example
	![[Pasted image 20241120143825.png]]
	Frames 3 4 and 6 are shared 
	All still have their own data though
How dynamically linked libraries work

Sometimes inadequate because the page table can become huge
Limit used to be 4 gb because 2^32 (32 bit machines) is 4 gb 

# Complex page tables
## Hierarchical page tables
Page tables divided into pieces (paged page table)
Done in order to handle larger page tables (32 bit to 64 bit)
Pages divided into 2 parts, p1 and p2
![[Pasted image 20241120144833.png]]
	p1 is an outer page table which points to their own page tables
	2^10 page tables 
	p2 points to the correct page table 
	Offset normal 
	2^32 points to 2^32 which has 12 bit offset
![[Pasted image 20241120145009.png]]
Its literally the same otherwise

Two level is not big enough for 64 bit
	We can increase to 3 level but that sucks because it is more memory accesses (slow)
	n levels require n+1 accesses

## Hashed page table
Used for >32 bit address spaces
Each entry has a linked list of elements with
	virtual page number
	page frame num
	next pointer
Algorithm
	Hash the page number into hash table
	Compare each element with each elem in the linked list until found 
	Form the physical address from the page frame
![[Pasted image 20241206133841.png]]


## Inverted page table
Tries to overcome drawbacks of big virtual address spaces like huge page tables and lots of memory

Aproach:
	Table has one entry for each physical page
	each entry has virtual page number for that frame (frame i has page number associated with it)
	![[Pasted image 20241206134442.png]]
	The process ID maps to a page and the index that the process id is at is the frame number

Lowers memory (does not store each page table) but uses O(n) search to find the pid, more there are the more time it takes
	Can use hashtable

# Segmentation
A way to manage memory that mimics the user view
	stack, heap, data, text all seperate "segments" 
![[Pasted image 20241206140210.png]]
Logical address is comprised of <segment num, offset>
Segment table maps logical addresses to physical addresses
	Base: Starting physical address where segments start in memory
	Limit: Length of the segment

Segment table base register (STBR): points to where the segment table is in memory
Segment table length register (STLR): number of segments being used by a program
	IF segment number >= s, then illegal segment

![[Pasted image 20241206140134.png]]

# Example Intel pentium
Uses both segmentation and segmentation with paging
CPU gen logical address -> segmentation unit -> linear address -> paging unit -> physical address

32-bit architecture 
Max number of segments is 16k
	8k in local descriptor table
	8k in global descriptor table

Max size of segment is 4gb ($2^{32}$ bytes)
Each segment descriptor is 8 bytes (has base and limit)
Each logical address is 48bit (16 to select segment, 32 offset)
	Selector has 13 bits of segment number, 1 bit table identifier, and 2 for perms
	48bit linear becomes 32bit logical
	![[Pasted image 20241206141153.png]]

Page size size is 4kb or 4mb
4kb:
	2 level paging scheme, p1 and p2 are 10 bits, offset is 12 bits
	Base of page directory pointed by CR3, accessed by first 10 bits
	Page directory gives base of inner page table, accessed by second 10 bits
	Final 12 bits is the offset
![[Pasted image 20241206141502.png]]

1. Find the location of the page that you want on the disk
2. If there is a free frame, use it. Otherwise use an algorithm to find a frame to replace (called the victim frame)
3. Bring the page into the frame that was freed up
4. Restart the process
To reduce the page fault overhead, use a modify bit that will only copy the page to memory if modified
![[Pasted image 20241208181905.png]]

# Page replacement algorithm
Goal: get the lowest page fault rate
Schemes:
	FIFO 
	Optimal page replacement
	Least recently used
	LRU approx
	Counting based replacement

To evaluate, we can simulate replacement on a string of memory page references and calculate the page faults

Expected graph ![[Pasted image 20241208182206.png]]
**Belady's anomaly**: Page fault rate sometimes increases when number of frames increases
## FIFO
Simple algorithm
	Each page has a time it was brought in, the one brought in first (oldest) is the one that gets removed first
	Replaced at head of queue, new page at tail of queue

Prone to belady's anomaly
![[Pasted image 20241208182606.png]]

## Optimal page replacement
Algorithm with the lowest fault rate
	Replace the page that won't be used for the longest amount of time
	No Belady's anomaly
	Provably optimal but needs future info
![[Pasted image 20241208183252.png]]
## Least Recently Used (LRU)
Approximates the optimal page replacement algorithm by keeping track of when each page is used and replacing the one that hasn't been used for the longest time
	Very good policy
	Requires hardware support in order to be fast and accurate
	No Belady anomaly	
![[Pasted image 20241208183417.png]]	
Multiple ways to implement

Counter Implementation
	Every page table entry has a field for *time of use*
	Copy the clock into the time of use field on every access
	Then when replacing, find the entry with the smallest value
	Requires full search and a memory write every time accessed
	COunter could overflow or take a lot of space

Stack implementation:
	Keep a stack of the page numbers in a linkedlist
	Move page to the top when it is referenced
	Remove bottom of stack
	No search required

Reference bit implementation:
	Associate reference bit with each page
	When referenced, set the bit. Clear it periodically
	Replace the one that has bit = 0 (if exists)
	Can't store the order of accesses

Record ref bit algorithm:
	Each page has ref bit and 8 additional bits
	Periodically, shift the 9 additional bits right and set ref bit as 0
	![[Pasted image 20241208184059.png]]

Second chance algorithm:
	Single ref bit as well as a basic FIFO (actually a circular LL but clsoe enough)
	The pointer goes through the FIFO looking for a ref bit 0, if there is one then the victim frame is found. Otherwise, if it is 1, then it is set to 0 and given "a second chance"
	If all are set to 1, then the first one checked would have been set to 0 first thus be used
	![[Pasted image 20241208184511.png]]

## Counting based algorithms
Counter of how many times a page is referenced
Least freq used: Replace the page with the smallest count
Most freq used: Replace the page with the largest count
Neither are very good

## Page buffering algorithms
Optimizes by Maintains list of victim frame
There is a buffer for the victim frames so that the new page is loaded immediately (both are in memory at the same time)
Scheme 1:
	Essentially, start running the program while the replaced page is being written to memory
	Don't need to wait until the replaced page is saved (async)
	(The victim page is copy-out)
	Page A gets buffered and then written into disk while page B is running
Scheme 2:
	The victim pages buffered until idle and that's when they are written
	(copy out when idle)
Scheme 3:
	Remember which page is in each victim frame (buffer them)
	If process wants a page that was a victim frame, it can just fetch it (useful for when a victim frame is used right after becoming the victim)

## Allocation of frames
How many frames does the OS need to allocate? Min? Max?
Min:
	Min depends on the max number of pages any instruction in the ISA can reference at once
		For direct addressing, this is 2 (one for the instruction, another for data)
		For indirect, 3 (instruction, address of data, data)
		Others might need more
	If min not provided, then it can't run properly
No real max, but don't want to give too much because it will slow down others

### Algorithm
Equal allocations:
	If there are *n* processes and *m* frames each process gets m/n frames
Proportional allocations:
	Get total number of frames (sum all frames), divide the process frame req with the total then mult by total number of frames
	![[Pasted image 20241209124247.png]]

Global replacement: Process selects one frame to replace from ALL frames 
	Process can take from another process
	Can't control its own page fault rate 
Local replacement: Selects process from its own frames to replace
	May be slower, but can control page fault rate

Non-uniform memory access:
	Processor has memory that is "closer" (not shared) which is lower latency and faster

## Thrashing
Process spends more time paging than executing
	Process keeps swapping pages in and out, high page fault rate
Usually because there aren't enough frames allocated to it

Cycle:
	Not having enough frames causes page faults
	This lowers CPU util
	OS will think it needs more compute so another process is added
	Even less frames for the processes so the cycle continues

Can prevent by reducing the number of active processes (so the process gets more frames)
Or by just allocating more frames

Multiprogramming leads to more thrashing
![[Pasted image 20241209125432.png]]

### Prevention

Prevent by just giving more frames (increase physical memory)
Working-set model:
	Estimates how much memory a process needs
	Based on locality model: Each process uses a small set of memory ref/frames, exec moves from one phase to another
		During one phase, the program is doing something similar
	The number of frames for each set is the *working-set*
Implementation:
	Assume a working set window $\Delta$ (moving window that can be 5ms for example, number of distinct pages used for each window is WSSi)
	WSSi (Working set size of process Pi) = total pages referenced in most recent $\Delta$
	Total demand is the sum of WSS(i)
	If D > m (frames) then thrashing is likely, suspend a process
	Working set has to be estimated

![[Pasted image 20241209141009.png]]
The page fault rate will spike at the start of the working set then fall until the next


Page-Fault Frequency scheme:
	Working set is complex and makes assumption
	Contrary to the previous cycle, instead of using CPU util for deciding more processes, use page fault rate (PFR)
	If PFR is too high, then lower the processes and/or increase the frames
	If PFR is too low, then increase the processes and/or decrease the frames
	![[Pasted image 20241209141952.png]]

# Memory mapped files
Rather than using syscalls to read/write to file, we map the disk to memory 
	Removes the syscalls
	Converts disk to memory access (faster)
	Simplifies disk

Mechanism:
	File read first using demand paging
	Page size chunk of file is read from file system into a physical frame
	After that the read/writes are just ordinary memory accesses

This allows several processes to map to the same file so the pages are shared
	![[Pasted image 20241209142730.png]]
	This is the method to share memory in some OS

There may be special IO instructions to transfer and control data to the IO controller (CISC)
Memory Mapped IO (RISC)
	IO device registers mapped to logical address spaces 
	Fast and good
		Fewer instructions (less IO complexity)
	Worse because you give up logical address space to IO devices
	Address spaces where you read/write that will go to IO device
May be control bit to know if data is available or not
	If CPU polls the control bit (checks regularly) then programmed IO
	If device sends interrupt then interrupt driven IO


# Allocating Kernel memory
Kernel memory is allocated from free memory pool
Does not use paging
	Some memory needs to be continuous 
	Want to minimize waste due to internal fragmentation
	Kernel requests memory for structures of very different sizes
Strats for managing memory (having contiguous without wasting)
	Buddy system
	Slab allocator

## Buddy system
Satisfies requests in units that are power of 2
Request rounded up to the next highest power of 2	
When smaller chunk is available, then split the chunk into two buddies of same size (div by 2)
	Continue until splitting would make it too small
21kb requested from 256kb
![[Pasted image 20241209144155.png]]
11kb of internal fragmentation

When done, the blocks will be combined


## Slab Allocator
Slab: Several physically contiguous pages
Cache has >=1 slabs
Single cache for each unique kernel DS
	Cache filled with instantiations (objects) of the DS
Slab allocation algorithm:
	Create cache of obj in cont space (mark as free)
	Store objects in free slots (mark as used)
	If the current slab is full, then allocate the next object from empty slab
	If no empty slab, go to free slots in next cache
Pros:
	No fragmentation
	Fast memory
![[Pasted image 20241209144932.png]]



# Virtual memory
Seperate logical memory from physical memory
Only part of the program needs to be in memory for execution
	Faster start up and creation
	More programs in memory at once (sharing address spaces)
	Programs larger than address space
Logical address space can be much bigger than physical address space


The space between the stack and the heap is unused (until heap/stack grows)
	Does not need physical pages for holes
Shared memory
	Pages can be shared so stuff like shared libraries can be loaded once (as read only) into address spaces
	Shared memory IPC easier too
	Parent pages shared with fork()


![[Pasted image 20241206142702.png]]

## Demand paging
Lazy swapper (pager): Doesnt swap page into memory until needed
	Less IO and memory
	Faster response
	More apps can be in memory

Page needed? Then reference to it
	If the reference is invalid, then abort
	If the page is not in memory, then bring it to memory


![[Pasted image 20241206142933.png]]

Each page table entry has a bit at the end called valid-invalid bit
	(v= valid, in memory and i=invalid, not in memory)
	default is i


Causes page fault if trying to access an i

![[Pasted image 20241206143548.png]]


### Handling page fault
1. References to data or instructions on an invalid page 
2. Memory access traps to OS  when page fault
3. Find a free frame in memory
4. Get the frame from the disk and load it into the free frame
5. Change the invalid page to valid and add to the table
6. Restart the instruction
![[Pasted image 20241206143926.png]]

Page fault rate is 0 if it never happens and 1 if it always happens
Effective access time (EAT) = $$(1-p) * \text{memory access} + p * \text{(page fault overhead + swap page out + swap page in + restart overhead)}$$
If memory access time is 200ns and page fault service time is 8ms then 
EAT = (1-p) * 200 + p * 8,000,000 = 200 + p * 7999800
	If p=.1% then EAT is 8.2us which is 40 times slower

### Process creation, copy on write
For when you call fork() which duplicates the address space
Copy on write (COW) allows sharing so both parent and child will share the same pages in memory
	Shared pages marked as COW pages
	If the page is modified, then the page is cloned
	ONLY the modified pages are cloned
	Free pages allocated from pool of zerod out pages
More efficient process creations 
![[Pasted image 20241206144756.png]]

## Page replacement
Demand paging review
	Seperates logical from physical like regular paging allowing for logical space > physical space
	page swapped only when needed
	Pros:
		Greater degree of multiprogramming
		Fast process start up
	Cons: 
		it can potentially over-allocate 
		May increase later memory access times
Overallocation is when currently active pages are more than physical space 
	Pages need to be replaced

Page replacement is for when a page needs to be added (page fault) but the physical space is full
Same page could be brought into memory many times so need to select the best one
1. Find the location of the page that you want on the disk
2. If there is a free frame, use it. Otherwise use an algorithm to find a frame to replace (called the victim frame)
3. Bring the page into the frame that was freed up
4. Restart the process
To reduce the page fault overhead, use a modify bit that will only copy the page to memory if modified
![[Pasted image 20241208181905.png]]

# Page replacement algorithm
Goal: get the lowest page fault rate
Schemes:
	FIFO 
	Optimal page replacement
	Least recently used
	LRU approx
	Counting based replacement

To evaluate, we can simulate replacement on a string of memory page references and calculate the page faults

Expected graph ![[Pasted image 20241208182206.png]]
**Belady's anomaly**: Page fault rate sometimes increases when number of frames increases
## FIFO
Simple algorithm
	Each page has a time it was brought in, the one brought in first (oldest) is the one that gets removed first
	Replaced at head of queue, new page at tail of queue

Prone to belady's anomaly
![[Pasted image 20241208182606.png]]

## Optimal page replacement
Algorithm with the lowest fault rate
	Replace the page that won't be used for the longest amount of time
	No Belady's anomaly
	Provably optimal but needs future info
![[Pasted image 20241208183252.png]]
## Least Recently Used (LRU)
Approximates the optimal page replacement algorithm by keeping track of when each page is used and replacing the one that hasn't been used for the longest time
	Very good policy
	Requires hardware support in order to be fast and accurate
	No Belady anomaly	
![[Pasted image 20241208183417.png]]	
Multiple ways to implement

Counter Implementation
	Every page table entry has a field for *time of use*
	Copy the clock into the time of use field on every access
	Then when replacing, find the entry with the smallest value
	Requires full search and a memory write every time accessed
	COunter could overflow or take a lot of space

Stack implementation:
	Keep a stack of the page numbers in a linkedlist
	Move page to the top when it is referenced
	Remove bottom of stack
	No search required

Reference bit implementation:
	Associate reference bit with each page
	When referenced, set the bit. Clear it periodically
	Replace the one that has bit = 0 (if exists)
	Can't store the order of accesses

Record ref bit algorithm:
	Each page has ref bit and 8 additional bits
	Periodically, shift the 9 additional bits right and set ref bit as 0
	![[Pasted image 20241208184059.png]]

Second chance algorithm:
	Single ref bit as well as a basic FIFO (actually a circular LL but clsoe enough)
	The pointer goes through the FIFO looking for a ref bit 0, if there is one then the victim frame is found. Otherwise, if it is 1, then it is set to 0 and given "a second chance"
	If all are set to 1, then the first one checked would have been set to 0 first thus be used
	![[Pasted image 20241208184511.png]]

## Counting based algorithms
Counter of how many times a page is referenced
Least freq used: Replace the page with the smallest count
Most freq used: Replace the page with the largest count
Neither are very good

## Page buffering algorithms
Optimizes by Maintains list of victim frame
There is a buffer for the victim frames so that the new page is loaded immediately (both are in memory at the same time)
Scheme 1:
	Essentially, start running the program while the replaced page is being written to memory
	Don't need to wait until the replaced page is saved (async)
	(The victim page is copy-out)
	Page A gets buffered and then written into disk while page B is running
Scheme 2:
	The victim pages buffered until idle and that's when they are written
	(copy out when idle)
Scheme 3:
	Remember which page is in each victim frame (buffer them)
	If process wants a page that was a victim frame, it can just fetch it (useful for when a victim frame is used right after becoming the victim)

## Allocation of frames
How many frames does the OS need to allocate? Min? Max?
Min:
	Min depends on the max number of pages any instruction in the ISA can reference at once
		For direct addressing, this is 2 (one for the instruction, another for data)
		For indirect, 3 (instruction, address of data, data)
		Others might need more
	If min not provided, then it can't run properly
No real max, but don't want to give too much because it will slow down others

### Algorithm
Equal allocations:
	If there are *n* processes and *m* frames each process gets m/n frames
Proportional allocations:
	Get total number of frames (sum all frames), divide the process frame req with the total then mult by total number of frames
	![[Pasted image 20241209124247.png]]

Global replacement: Process selects one frame to replace from ALL frames 
	Process can take from another process
	Can't control its own page fault rate 
Local replacement: Selects process from its own frames to replace
	May be slower, but can control page fault rate

Non-uniform memory access:
	Processor has memory that is "closer" (not shared) which is lower latency and faster

## Thrashing
Process spends more time paging than executing
	Process keeps swapping pages in and out, high page fault rate
Usually because there aren't enough frames allocated to it

Cycle:
	Not having enough frames causes page faults
	This lowers CPU util
	OS will think it needs more compute so another process is added
	Even less frames for the processes so the cycle continues

Can prevent by reducing the number of active processes (so the process gets more frames)
Or by just allocating more frames

Multiprogramming leads to more thrashing
![[Pasted image 20241209125432.png]]

### Prevention

Prevent by just giving more frames (increase physical memory)
Working-set model:
	Estimates how much memory a process needs
	Based on locality model: Each process uses a small set of memory ref/frames, exec moves from one phase to another
		During one phase, the program is doing something similar
	The number of frames for each set is the *working-set*
Implementation:
	Assume a working set window $\Delta$ (moving window that can be 5ms for example, number of distinct pages used for each window is WSSi)
	WSSi (Working set size of process Pi) = total pages referenced in most recent $\Delta$
	Total demand is the sum of WSS(i)
	If D > m (frames) then thrashing is likely, suspend a process
	Working set has to be estimated

![[Pasted image 20241209141009.png]]
The page fault rate will spike at the start of the working set then fall until the next


Page-Fault Frequency scheme:
	Working set is complex and makes assumption
	Contrary to the previous cycle, instead of using CPU util for deciding more processes, use page fault rate (PFR)
	If PFR is too high, then lower the processes and/or increase the frames
	If PFR is too low, then increase the processes and/or decrease the frames
	![[Pasted image 20241209141952.png]]

# Memory mapped files
Rather than using syscalls to read/write to file, we map the disk to memory 
	Removes the syscalls
	Converts disk to memory access (faster)
	Simplifies disk

Mechanism:
	File read first using demand paging
	Page size chunk of file is read from file system into a physical frame
	After that the read/writes are just ordinary memory accesses

This allows several processes to map to the same file so the pages are shared
	![[Pasted image 20241209142730.png]]
	This is the method to share memory in some OS

There may be special IO instructions to transfer and control data to the IO controller (CISC)
Memory Mapped IO (RISC)
	IO device registers mapped to logical address spaces 
	Fast and good
		Fewer instructions (less IO complexity)
	Worse because you give up logical address space to IO devices
	Address spaces where you read/write that will go to IO device
May be control bit to know if data is available or not
	If CPU polls the control bit (checks regularly) then programmed IO
	If device sends interrupt then interrupt driven IO


# Allocating Kernel memory
Kernel memory is allocated from free memory pool
Does not use paging
	Some memory needs to be continuous 
	Want to minimize waste due to internal fragmentation
	Kernel requests memory for structures of very different sizes
Strats for managing memory (having contiguous without wasting)
	Buddy system
	Slab allocator

## Buddy system
Satisfies requests in units that are power of 2
Request rounded up to the next highest power of 2	
When smaller chunk is available, then split the chunk into two buddies of same size (div by 2)
	Continue until splitting would make it too small
21kb requested from 256kb
![[Pasted image 20241209144155.png]]
11kb of internal fragmentation

When done, the blocks will be combined


## Slab Allocator
Slab: Several physically contiguous pages
Cache has >=1 slabs
Single cache for each unique kernel DS
	Cache filled with instantiations (objects) of the DS
Slab allocation algorithm:
	Create cache of obj in cont space (mark as free)
	Store objects in free slots (mark as used)
	If the current slab is full, then allocate the next object from empty slab
	If no empty slab, go to free slots in next cache
Pros:
	No fragmentation
	Fast memory
![[Pasted image 20241209144932.png]]


