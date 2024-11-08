# Ch 4
1. With fork, the calling thread is forked but with exec, all threads are overwritten
2. Asynchronous which is immediate end and deferred which waits until the program stops itself
3. Synchronous signals come from inside the program and Asychronous come from outside the program. For both, the OS can handle itself or give it to the program to handle. It can also be per a single thread, for all threads, some, etc.
4. Thread pools are there to eliminitate the startup time of the thread by keep one made but dormant. That way when it is needed it is faster to start. OpenMP is there to be safer and easier to use. Lots of the multithreading is abstracted

# Ch 5
1. The critical section is an area of memory that multiple threads are accessing at once. Mutual exclusion, only one thread accessing at once. Progress, if there is no one in the critical section, the thread gets access and doesn't have to wait. Bounded waiting, a thread isn't waiting inf for its turn
2. Bounded waiting, there is no set time that a process will be waiting
3. dec, inc, dec, if, inc, print
4. wait before dec and after if you signal
5. Bounded waiting, mutex, and progress ??
6. Flag in the while loop checks itself not the other process
7. In slides
8. In slides
9. Semaphores are global variables used for mutual exclusion. Simple semaphores are either counters or binary and while waiting, they are spinning (busy waiting). Semaphores with no busy waiting sleep the program and get added on to the stack instead of waiting. This has the additional overhead of context switch and the busy waiting is moved to the part that puts the process to sleep
10. Priority inversion happens when there is a higher priority process waiting on a lower priority process for a lock but a medium or higher priority process jumps the lower priority one. This causes the highest priority to have to wait for med and low. Priority inheritence increases the priority of the lower one to match the higher one so it cant get jumped.
11. Its because context switching for multicore processors is more expensive than spinwaiting so it is better to spin wait but with uniprocessors this is not the case so sleeping is better
12. They are low level constructs that are essentially just a global var. They are also hard to use and do right. There is also no control over proper usage
13. Monitors implictly ensure mutex because they only allow one process in the critical section at a time ???
14. Hoare: C1 P1 Mesa: P1 C1
15. In monitors, the wait gives up the lock and the signal is lost if there are no waiting threads. In semaphores all signal does is inc global var
# Chapter 6
1. Turnaround, waiting, deterministic modeling
2. Preemptive means the process involuntarily leaves while nonpreemptive means the process only stops executing if it wants to. When recieving input from a user, preemptive provides more response time because waiting for IO can be switched in and out with other processes
3. a
FCFS: 
	P1 0-3, P2 3-10, P3 10-12, P4 12-16, P5 16-17
	P1 3, P2 10, P3 12, P4 16, P5 17
	P1 0, P2 3, P3 10, P4 12, P5 16
SJF: 
	P5: 0-1, P3 1-3, P1, 3-6, P4: 6-10, P2 10-17
	P5 1, P3 3, P1 6, P4 10, P2 17
	P5 0, P3 1, P4 
Priority: 
	P1 0-3, P2 3-10, P3 10-12, P5 12-13, P4 13-17
	P1 3, P2 10, P3 12, P4 17, P5 13
RR:
	P1 0-1, P2 1-2, P3 2-3, P4 3-4, P5 4-5, P1 5-6, P2 6-7, P3 7-8, P4 8-9, P1 9-10, P2 10-11, P4 11-12, P2 12-13, P4 13-14, P2 14-17
4. If throughput is most important and response time is less, then I would use longer quantums because that means less context switching overhead
5. Im not doing allat again
6. a) b) 4 10 10 10 2 6 10 10 6 4 4 
7. SO that processes that are lower priorty that have been waiting forever will have their priorities raised so starvation doesnt happen
8. There are multiple queues each with different priorities and/or scheduling algorithms. This is done to either differenciate background/foreground jobs or for used for scheduling to have more fine tuned control so shorter ones are met first
9. a) each short will have 5/35 and long will have 2/35 b) each short will have 5/25 and each long has 2/25
10. Process ??