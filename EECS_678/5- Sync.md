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

# 0 is lock