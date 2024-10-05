### OS design
Design
	Type of system: batch, time-shared, amt of users, distributed, real time, embedded
	User goals: convenience, ease of use, reliable, safe, fast
	sys goals: ease of design, implementation, maintanance, flexible, reliable, error-free, effecient
Mechanism 
	Policy: what to do
	Mechanism: how to do it?
Implementation:
	high level lang: easier, faster to write, easy to debug, portable
	Assembly: more effecient 
### Structure
OS needs to be modular, maintaibible, sustainable, 
Simple strcuture: 
	Characteristics
		Monolithic
		Poor seperation between interfaces and levels of functionality 
		Ill suited design, difficult to maintain and extend
	Reasons:
		Growth beyond original scope and vision 
		lack of neccessary hardware features when designing
		Guided by hardware constraints
	MS DOS
	![[Pasted image 20240906000610.png]]
	The application program (you) can access the drivers and lower levels
Traditional UNIX architecture:
	![[Pasted image 20240909142222.png]]
Layered:
	OS is divided into modular layers
	Upper layers use the functions that run in the lower layers
	Pros:
		 More modular, extensible, maintainable
		 Hides info! (higher level does not see what goes on in the lower level)
		 Simple to contruct, debug, and verify (if upper level throws bug look at the function it uses)
	Cons:
		Less effecient 
		It is harder to seperate functionality cleanly
	![[Pasted image 20240909142552.png]]
	The user can only access the layer beneath it
Microkernel:
	Make the kernel as small as possible by moving functionality into the user space
	Communicate between user modules (itself) using message parsing
	Pros:
		Easier to extend (just add more user level drivers)
		Easier to move to new architectures since the kernel itself is so small
		More reliable and secure (again since kernel is as small as possible)
	Cons:
		No consensus for services that should stay in the kernel and not
		Performance overhead because user talks to kernel (lots of context switching)
		