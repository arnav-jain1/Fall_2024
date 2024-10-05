# Process
Process aka job:
	Program in execution
	Instance of a program that is being executed sequentially 
	Must be executed sequentially
Program
	passive entity, not being executed
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
	*Context* represented in PCB
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
	Memory 2 process is shared and can be read/written to 
	Fast, convinient 
Message parsing
	Send and receive messages
	Messages not overwritten so no conflicts
	Slower but better for multiple computers
	Easier to implement 
	Typically used for smaller amounts of data
![[Pasted image 20240918173407.png]]

Message parsing can also be used for client-server communication 
2 operations: send and receive (a message)
If P and Q want to communicate, they need to first establish a link before exchanging messages
Implementation issues:
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
	Issue: Process IDs are hard coded

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

Issues:
	Unidirectional
	Only constructed in parent-child or child-child relationship
		Only exist until the process exists so if the process terminates early, data might be lost
		AUtomatically deleted when process ends
	Must be controlled by same OS (cant be used over a network)
	Data only FIFO
	
fd0 is stdin, fd1 is stdout and fd2 is stderr so when you create a new pipe. the file descriptors for reading for the pipe is fd3 and writing is fd4

When you create a fork after already making file descriptors, the child gets the same one so then the child can write to it and the parent can read from it

What does | do? 
	example: /bin/ps -ef | /bin/more
		Create a process to run `ps ef`
		Create a process to run `more`
		Create a pipe from `ps ef` to `more`
		The stdout of `ps ef` is redirected into the pipe
		The stdin of `more` is the output of the pipe

## FIFO (named pipes)
Pipes with a name
More powerful than anonymous (regular) pipes
	No parent-sibling relationship needed
	Bidirectional 
	Can persist after process terminates
Characteristics:
	Appear as regular files
	half duplex
	communication must be on same machine

## Message Queues
Linux uses indirect communication or mailboxes
There can be multiple processes with queues (sychronization may be needed)
Processes can use any number of queues (each queue is unique)
Capacity of the link is initialized by the system (can be overridden by user)
Each message has a length which is specified in send and receive calls
Each process can send and recieve calls from the same queue

Message queues is how Linux sends and receives messages
msgget: Create a new message queue
msgsnd: Send a message to the queue
	struct msg_buf { long mtype; char mtext\[]}
	Non blocking unless queue is full
msgrcv: Receive message from queue (mtype to get specific messages)
msgctl: Control operations for the queue (like terminating)

## Memory sharing
multiple Processes can utilize the same chunk of memory
Implementation principles:
	Name unique (system wide) or anonymous 
	Specifying permissions (read, write, execute)
	Dealing with race conditions (atomic or synchronized access)
	Most thread level communication is from shared memory
Example:
	shmget: create shared memory segment 
		Requires size and returns identifier
		Same access perms as files
	shmat: attach shared memory segment
		Must for every process that wants to access it
		Identified by segment id
	shmdt: Detach shared memory
	shmtcl: Control operations (like removing)
## Unix sockets
Sockets:
	End point for communication 
	Two way communication pipe
	Variety of domains like the internet
Unix domain sockets:
	Communication between processes on same Unix system
	Special file in the system
Mostly for client-server programming
	Client sends a request for info (like API calls)
	server waits for requests then does the request and sends info (updates, output) to client
Modes:
	Connection-based: TCP
		Heavyweight, makes sure that the packets are in order with none lost
		Slow
	Connection-less: UDP
		Faster, does not care for order/drops
Syscalls:
	socket(): make the Unix socket
		int socket(int domain, int type, int protocol)
	bind(): Assign an address to a socket int bind(int sockfd, ... \*addr, adderlen)
		A file. also what you connect to when you go to a website
	listen(): listen to incoming client requests
		int listen(int sockfd, int backlog)
	accept(): Create a new connected socket
	recv(): Receive messages from socket (message placed in buf)
	close(): Closes the connection
	