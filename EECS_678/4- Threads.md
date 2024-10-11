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
	TEsting and debugging