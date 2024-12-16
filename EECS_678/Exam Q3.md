# Ch 8 
1. Base and limit registers are used to keep bounds for processes. A singular process cannot go above or below these bounds creating a range for each individual process where the range cannot be exceeded
2. Compiler time address binding is setting the addresses when compiling. This can cause issues if time has passed and things have changed. Load time is setting the addresses when loading the program into memory. In both of these the physical address and the logical address is the same and there is minimal overhead. Execution time is when executing the addresses are calculated which has overhead
3. Logical is what the CPU uses and physical is the actual address in the RAM
4. Dynamic linking is linking routines or obj files only when needed. For now there is a code stub and when it is needed it is linked. This lowers the amount in memory so like for stdio, it is only in memory once and every process that uses it will just refer to that one instance
5. Swapping allows for higher amount of virtual memory than there is physical memory. When memory runs out, the page can be stored on the hard drive
6. Leads to a lot of fragmentation and holes being created ?
7. First fit is the first hole is used, best fit is the hole with the smallest leftover hole and worst fit is the hole with the largest leftover hole
8. Internal fragmentation is when more space is allocated than needed. External is when holes are there but are too small/scattered to fit another process
9. Paging is creating blocks where each process is assigned a block that can be anywhere. The memory is non contiguous so external fragmentation is not an issue 
10. 11 for the offset, 21 for page number. Because it makes it easy to calculate since each bit represents a power of 2
11. a) 100ns b) 15 * .9 + .1 * 115 = 25ns
12. Simple page table is ineffective when there are too many pages and it takes up a lot of memory a) hierarchical is creating a page table for other page tables b) hashed is hashing the pages and then keeping a linked list for all the pages that hash to the same number c) Inverted is having a row for each frame and the number of the row is the frame that it corresponds to. The index to search is the pid
13. Segmentation can be of any size instead of fixed sizes
14. Best fit is the best
	1. First fit: 212 goes into 500 (becomes 288), 417 goes into 600, 112 goes into 500 (is actually 288 now) and 426 leftover
	2. Best fit: 212 goes into 300, 417 goes into 500, 112 goes into 200, and 426 goes into 500 
	3. Worst fit: 212 goes into 600, 417 goes into 500, 112 goes into leftovers of 600, and 426 left over
15. 
	1. 2375: Page 2 (0 indexed) offset of 322
	2. 19366: Page 18 offset of 934
	3. 30,000: Page 29 offset of 304
	4. 256: Page 0 offset of 256
	5. 16384: Page 16 offset of 1
16. a) 2^20 b) 2^29 
17. a
	1. 0,430: 649
	2. 1,10: 2310
	3. Seg fault
	4. 1427
	5. Seg fault


# 9
1. Virtual memory is there so that processes can use more memory than what is actually available. It is also there to incorporate paging in order to reduce external fragmentation
2. Demand paging is only bringing in pages to virtual memory when needed
3. When a page needs to be accessed thats not in memory. 
4. When using paging 
5. Lower bound would be N, Upper bound would be P
6. When increasing frames increases page faults

12. 
	1. Thrashing, decrease multiprogramming
	2. Perfect
	3. Increase mult programming
13. Too big of a page size increases internal frag while too small increases the size of the page table which increases the overhead
14. Benefits is that it "increases" memory available but the downside is that it can be slow, has more overhead, and can lead to unintended slowness due to thrashing or using more memory than available 
15. So the page size is 4096 = 2^12 so the first 12 bits will be used for offset so the offset is 0x456. Then the rest will be used for page number so 0x11123 will be the page number. This is mapped to some frame in physical memory which 0x456 is added to to get the final physical address
16. Gonna assume it is 256 bits not bytes because then the offset would be 11. 256 bits means offset is 8 and page is 4
	1. 9EF: Page 9 offset EF so 0x0EF
	2. 111: Page 1 offset 11 so 211
	3. 700: No page so D is first free so D00
	4. 0FF: No page so next is E so EFF
17. The cause of thrashing is not enough frames so page faults are constantly happening. It can be detected because the CPU util is low but disk util is high. It can allocate more frames
18. 