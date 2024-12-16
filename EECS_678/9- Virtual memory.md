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


