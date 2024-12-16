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
10. 11 for the offset, 21 for page number. 