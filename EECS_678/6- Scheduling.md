# Basic concepts
Multiprogramming
	Alternate between CPU and IO bursts
		IO burst: Process does IO and not use CPU (free/idle)
		CPU burst: Process does CPU and not use IO
	Can schedule another process during IO burst (max the util)
CPU bound
	Speed bounded to CPU speed (better cpu = faster)
	Most time is doing CPU, long CPU bursts
IO bound
	Spend most time doing IO
	Does not depend on CPU speed
	Few short CPU bursts

# Scheduler
Picks the next process to run
	Part of the OS *dispatcher*
	Selects processes from memory that are ready to execute
	Utilizes a strategy
Happens when process switches from
	1. running to waiting (when IO)
	2. running to ready (time slice is up)
	3. waiting/new to ready (check if the priority is higher than currently running)
	4. terminates
All 1 and 4 is nonpreemptive (process voluntarily finishes), others are preemptive (Involuntarily)

Nonpremptive:
	Process voluntarily is done with CPU
	Easy, no special hardware (no interrupts, timers)
	Bad response time for interactive and real-time systems
Prememptive (better):
	OS forces process to leave  the CPU (higher priority or time slice runs out)
	Special hardware like timer
	Needs to maintain consistency 
	Complicated but preferred
	Favored by OS


## Dispatcher
Scheduler is part of dispatcher
Goals:
	Get new process from scheduelr
	Context switch (remove current process)
	Give CPU new process
	Jump to right spot in new program to restart 
Time taken to do this is called dispatch latency


## Scheduling queues
Job queue: All processes 
	Long term scheduler pareses them and brings them into memory 
	All processes are in job queue
Ready queue: Processes in memory
	Ready and waiting for execution
	Scheduled by short-term scheduler
Device/IO queue: waits for a device
	Process can be blocked for same device 
	IO completion moves back to ready queue

![[Pasted image 20241028141703.png]]
Ready queue is the queue that is waiting to be processed
Can escape the process if:
	IO request, then goes to IO queue and once doen then goes to ready
	Time slice: Put back in ready
	Fork: When child done, goes back to ready
	Interrupt wait: When interrupt occurs, goes back to ready

## Metrics
How to measure how good each scheduling algo is 
Maximize:
	CPU Util: % of time CPU is busy (includes busy waiting)
	Throughput: Number of processes that finish per time unit
Minimize:
	Turn around time: How long it takes to finish a process once submitted
	Waiting time: How long a process is waiting in the ready queue
	Response time: Amount of time it takes from request submitted until resposne
Also want to be fair to all processes and users


Evaluation criteria:
	Give importance (or weight) for each metric
Deterministic modeling (what we use):
	Take a workload (group of processes) and gets the perf results for each algorithm
	Simple and fast w exact numbers
	Difficult to generalize (what tradeoffs are good/bad may vary)
	Shows algo trends

Workload model:
	![[Pasted image 20241028142738.png]]
	Shows list of processes and information
	Process 1 arrived at 0 and did a CPU burst for 8s
	P2 arrived at 1 and burst for 4
	P3 arrive at 1 (but exec after because P2 is first) and burst 10
	P4 arrived at 6 and burst for 2
<mark style="background: #FF5582A6;">Does this mean it happens at same time?</mark>
Gantt chart:
	For batch scheduling algorithm (non premp)
	![[Pasted image 20241028143022.png]]
	P1 starts, exec for 8 then P2. P2 exec for 4 then P3 for 10 then P4 for 2

Same workload model will have different gantt charts

![[Pasted image 20241028143342.png]]
For process A:
	Submitted at 0
	Response time is 0
	Turnaround time = 9
	Wait time is 2+2+1 = 5 (time it is not running and not done)
For process B:
	Submitted at time 0
	Response time is 1
	Turnaround time (submission to completion) = 5
	Wait time = 3


Queuing model:
	Theoretical model
	Represented by a bunch of equations 
	Depends on a bunch of assumptions
Simulations:
	Most common, best
	Simulate a coded scheduling algo based on a bunch of data
	Time and space intensive
	Not always deterministic (small variations, systems are complex)
	Overhead of algorthim (how long it takes to decide) is considered

![[Pasted image 20241028143840.png]]
Get trace of a real system (list of processes ran inlcuding cpu burst, times, etc) and then run the trace


# Algos
## First come first served
The first request is the first served (FIFO)
No preemption 
	No time slice
Advantages:
	Easy to write
Disadvantages:
	waiting time might be long
	Doesn't balance IO bound and CPU bound processes
	Convoy effect: Long process before short process so the average wait time goes up
	Not usable for time sharing systems
![[Pasted image 20241030140814.png]]
P1 will run from 0 to 24, then P2 from 24 to 27 and then p3 from 27 to 30
Waiting time $WT(P_{1})$ for $P_{1}$ = completion time - submission time - time burst (running time)
	= 24 - 0 - 24 = 0
$WT(P_{2})$ = 27 - 0 - 3 = 24
 $WT(P_{3})$= 30 - 0 - 3 = 27
Average wait time = (0+24 + 27)/3 = 17

Turnaround time for $P_{1}$ = Completion time - submission time
	= 24 - 0 = 24
$TT(P_{2}) = 27 - 0 = 27$
$TT(P_{3}) = 30 - 0 = 30$
$Av(TT) = \frac{24+27+30}{3}=27$



If the order is $P_{2}, P_{3}, P_{1}$
This improves everything significantly
![[Pasted image 20241030141718.png]]
Issue is that the average is very variational, and no preemption (bad for interaction and real time systems)
But the scheduling is constant time, it is fast and scheduling the job is fast and always the same

## Shortest job First (SJF)
Order each of the processes based on how long their burst is (how long it takes to run)
The shortest job runs first
Advantages
	SFJ is a greedy algorithm and optimal because the wait time will always be the shortest possible 
	Good benchmark
Disadvantages:
	Difficult to know length of the next CPU request (user might not even know) so it is difficult to implement (unrealisitc)
	Leads to process starvation if there is a process with a long CPU burst and there are always shorter processes 

![[Pasted image 20241030142407.png]]
P4 (3) runs from 0 to 3 -> P1 (6) runs from 3 to 9 -> P3 (7) runs from 9 to 16 -> P2  (8) runs from 16 -> 28

$WT(P_{3}) =$ 16 - 0 - 7 = 9
Av wait time = 7
We know that 7 is the BEST wait time

Since we don't know how long it will take, we need a way to predict it
### Estimating length of next CPU burst
Done by looking at CPU burst of how long the process took in the path
	Calc as exponential av
$t_{n}$ = actual length of nth CPU burst
$\tau_{n+1}$  = predicted value of the next burst, random at the start for n=1
$\alpha, 0 \le \alpha \le 1$ 
$\tau_{n+1}= \alpha t_{n} + (1-\alpha)_{\tau_{n}}$     
If $\alpha = 0$, then history does not count, random guess
If $\alpha=1$ then only the last CPU burst counts

$\tau_{n+1} = \alpha t_{n} + (1-\alpha)(\alpha t_{n-1} + (1-\alpha)(...))$   so the previous generations are weighted less and less because the $\alpha$ gets squared for the gen before the prev, cubed for the one behind
![[Pasted image 20241030143957.png]]
Inital guess was 10, actual was 6
so $\tau_{2} =.5*6 + .5 * 10=3+5 = 8$
Actual $t_{2}=4$ 
so $\tau_{3}= .5 * 4 + .5 * 8 = 6$
...

One of many models


Adding preemption to SJF is when a new shorter process comes in