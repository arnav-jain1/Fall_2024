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

### Discresionary Access Control (DAC)
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
	