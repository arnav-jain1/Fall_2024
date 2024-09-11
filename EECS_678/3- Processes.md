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