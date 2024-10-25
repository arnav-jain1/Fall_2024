# Access control
OS manages resources so it controls access to CPU, RAM, Storage, protection mechanisms, etc.

Access control is a way to regulate who can do what to which
1. Who is allowed to *certain* (which) resources (Who can access root?)
2. What actions can they do (read/write/exec)
3. Under what circumstances (always, sometimes, never?)
Access control policy specifies authorized accesses
	Rules that control how the access rights are assigned and enforced
	Based on roles, attributes, or other criteria


## Closed vs Open policy
Closed policy: Denied unless explicitly given
	Start with no access until granted
	Also known as default secure
Open policy: Allowed access until explicitly denied
	Essentially the inverse
	All access until removed


## Access control Modes
Access control modes:
	Subject: entity that requires access to an object
	Object: The entity trying to be accessed 
	Access rights: how the subject is allowed to access the object

### Discretionary Access Control (DAC)
Standard in OSes and very common in multiuser platforms like DBs, OS

Concept: User protects what they own
	Owner has perms to access objects
	Owner can grant/revoke access to others
	Owner decided what access others get (r/w/x)

Subject: user ID or a process 
	Authentication: OS verifies the user identity (login name/pswd)
	Authorization: Decides whether access is allowed/denied



#### Implementation
Access control matrix 
	Each user is a row, col is obj
	Ineffecient due to lots of empty spaces and can become pretty big
	If someone accesses this then its screwed
![[Pasted image 20241023112732.png]]

Access control directory 
	Describe permissions for every user
	Access write and pointer for each file
	List can get very long (same item in multiple lists)
	Difficult to revoke
	Inconsistent perms due to file pseudonym 
![[Pasted image 20241023113107.png]]

Access control List (ACL)
	Maintain list per object instead of per user
	Sorted list of 0+ Access control entries (ACE)
		ACE specifies what users/groups have which perms for each file
![[Pasted image 20241023113719.png]]
	Supports default perms, all users can access PUBLIC files
	Can also create groups to grant perms to instead of each person individually

#### Unix/Linux
DAC: Owner of file can change the perms
Closed Policy: All perms that are not granted explicitly (chmod) are denied (no way to explicitly deny)
Path-based access controls: All dirs in the path to the program must have execute and program must have r to access a file
Other than rwx, Linux has extended perms like append only
3 basic ACEs: owner, group, other

In file has a set of bits that signify the permissions
	12 in UNIX
		**3 bits for r/w/x for user**
		**3 bits for r/w/x for group**
		**3 bits for r/w/x for all others**
		1 sticky bit for specifying only owner can rename/mv/del a file
		2 bits for setuid and setgid perms for privileged access. When you execute the file, run it as the owner of the file or the specified group
![[Pasted image 20241023114557.png]]
		

Usually, users wont make changes that will affect other users but sometimes, you need to install/fix things
	Becomes superuser, account with more privileges (root for linux, administrator for windows)


Modern OS have extended ACL (free BSD still has 9)
	setfacl assigns list of perms for users/groups to a file
	mask has maximum perms for any user, cant exceed those perms
![[Pasted image 20241023115423.png]]

#### Windows
Uses ACLs:
	each access control describes user/group and perms allowed/denied
	Also audit permission that denotes logging
	Same mechanism for registry 

#### Implementation
privalege lists
	row of access control matricies that have access rights for a *subject*
	easy to revoke
	The point of this is to create unforgeable token that gives processes access rights to an object which the user recieves and then can pass down
Protection domain: Collection of subjects that the user has access to 
	All the capabilities is the domain (row of matrix), can include, IO, files, programs, etc
	Can define new domain for any process that has a subset of access rights
	There is *seperation* meaning different domains for different users/processes (unix has this with user and kernel mode)
	Supports least priv by minimizing access rights that any user has at any time
	
### Problems with DAC
No control on flow of perms
Subordinate creates a file that only has write permissions tricking the owner and not allowing them to see

Trojan horses try to bypass OS access control so someone tricked into granting a trojan perms will be screwed


Defense: Trusted OS
	Assign mandatory access control policy for the TCB with security levels to the files
	Reference monitor to check all access requests
	For example, making it so that you can't write to lower security level so you are not tricked into writing at the wrong spot

No fine grained access control: Need multiple groups which can be complicated and large. 
	Use extensions to enforce fine grained access control


Setuid issues: If set, system will temporarily use program owner's priv so we need to watch the ones owned by superuser/root when setting the bit otherwise it could get bad

Caused because:
	Process has 2 UIDs, real and effective
		Real identifies the owner of process
		effective identifies priv of program, access control based on this
	When you run a program, EUID = user who runs the program
		EUID of user process is user no matter if you set owner to root
	When SetUID, then  EUID is the owner of the program
![[Pasted image 20241023121329.png]]
