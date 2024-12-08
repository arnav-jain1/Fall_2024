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

1. Find the location of the page in the disk