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
Stack is automatic, local vars and is only there during the function
Heap is for dynamically allocated data, explicit or implicit (depending on language), scope continues until deallocation
Data: global and static data, allocated/deallocated on process creation/termination, scope is during execution
Text: Program binary instructions (sequential instructions from assembly/C) with same properties as data
![[Pasted image 20240911142409.png]]
Data and text dont change. Stack moves down per function call and heap is for dynamic allocation

There is a limit to how much space you are allowed on the stack (you can change it if you want

Process states:
- New: Process being created
- running: instructions being executed
- waiting: waiting for an interrupt
- ready: waiting to be processed by CPU
- Terminated: Done
![[Pasted image 20240911143809.png]]

# Context switch
Process of storing and restoring the state (*context*) of CPU so that multiple processes can share a CPU
	Time shared, multiprogramming env
	*Context* represented in PCB
	Save current process then restore the next process
	Switching from user <-> kernel is a mode switch
Overhead because no other work is done when switching

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
	OS has *primordial* process
	creating process is *parent* while created process is *child*
		If parent dies before child, the child process becomes an *orphan* 
	Processes IDed by process identifier PID 
Resource sharing options
	1. Parent and child shares resources 
	2. Children share subset of parent process resources
	3. Parent and child doesn't share resources (Unix)
Execution options
	1. Parent and child run together (unix)
	2. Parent waits till children finishes 
	Parent process has to collect the child process status when the child process is finish
		If it doesn't collect it, the child process becomes a zombie process
	If Parent process finishes first, it will either be orphaned or zombied
	All the parent's responsibility is to collect the status of the child when done
Address space options
	1. child duplicate of parent
	2. child has a program loaded into it by exec
UNIX examples
	Fork system call will create a new process
	Exec will replace the process' memory space with a new program

Parent waits for its child to finish
![[Pasted image 20240917175545.png]]

## Process termination
Process ends after the last statement executes
	The process itself can explicitly call **exit()** but if it doesn't, then the OS implicitly calls it.
	Child can then pass it's return status to the parent that collects it using **wait()**
	Resources are then dealloc by OS
Parent can terminate the child process explicitly using **abort()**
	Like if child is using too many resources or is not needed
Is parent exits then:
	The child will be assigned a new parent by the OS and can run independently  (Unix)
	Might not be the case for other OS
