# Sychronization
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
If mutex is false, it is set to true and will continue through the while loop since it returns false
If it is true, then it will continue