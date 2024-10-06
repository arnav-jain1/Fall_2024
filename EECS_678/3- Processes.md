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