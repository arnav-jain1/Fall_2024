# Chapter 1  
1. Explain the User’s and System’s view of the operating system.  

2. Explain the operating system goals.  

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

8. What factors restrict the size of the cache? Why have multiple levels of caches?  

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
7. What is the purpose of the fork and exec system calls?  
Fork is to create a new process and exec is to run a command like ls that will override the rest of the program
8. Using appropriate system calls, have the program below create a new process, and execute   the program /bin/ps. It is not important to get all the function arguments and their   positions correct.  
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
User level
4. Name and illustrate the three multithreading models.  
5. What is the main disadvantage of the many-to-one multithreading model?  
6. What system call is used to create threads in Linux?  
7. Write a simple program that contains two global variables num1 and num2. The program  
then: (a) creates two threads, (b) Thread-1 performs num1+num2 and returns the result,  
(c) Thread-2 performs num1-num2 and returns the result, (d) Main thread displays the two  
returned results and exits.  
8. How do system calls fork and exec operate if invoked from multi-threaded programs? (You  
may use Linux as an example.)  
9. Explain the two mechanisms of thread cancellation.  
10. How are synchronous and asynchronous signals handled in a multi-threaded program?