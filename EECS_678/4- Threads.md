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

# Threads
Lightweight process with:
	Independent flow of control
	but still shares resources with other sibling threads
	Lives in the same context space as the process
Thread shared data:
	Process instructions
	most data (the global vars part of address space)
	File descriptors 
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
		Popular before 2005 because CPUs were all single core, now if thread is blocked, kernel will only see one process so it is hard to take advantage of multiple cores
Kernel level threads: Thread at kernel level
	Pros: 
		Removes disad of user level
	Cons:
		More overhead because syscall needed (and context switch)
		Provided by all OS (Mac, windows, linux)

# Multithreading models
Many-to-one
	Many user level threads mapped to a single kernel level thread
	Bad because if one user thread is blocked, so is the rest of the threads
	Bad for multicore CPUS
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


END SLIDE 21