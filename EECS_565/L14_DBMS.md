# DB
Dbs are a collection of data and rules to organize and connect the data 

Different layers, want security for all:
	Data: Want integrity (trustable) and enforce privileges 
	Data files: Protect by permissions and encryptions 
	OS: Gateway to data, provides authentication and authorization for data
	DBMS: logical structure 
	Network: Access point to DB
	Application: Privileges and perms granted to people
	People: Users with perms to access applications, db, files, and data


## Objectives
Protect sensitive data from: 
	Unauthorized disclosure (not allowing people who aren't allowed to see it)
		Done with user authentication, access control, auditability 
	Unauthorized modification (Not allowing other things to modify it)
		Physical, logical, element integrity 
	Denial of service attacks:
		Availability 


Security controls:
	Security policy 
	Integrity protection 
	Fault tolerance
	auditing and intrusion detection
	access control modes
	Privacy problems and control

# Integrity
DBMS and OS responsible
OS:
	User account management
	Authentication
	Access control
	Integrity check for I/O devices
DBMS:
	Update: error/failure might occur while updating data, want to prevent anything from going bad if happens. Done via 2 phases, intent and commit phase
	Recovery: Log of uses/access so if it crashes we can recover
	Redundency: detect internal inconsistency in data. Use *shadow fields* to error correct to check integrity
Concurrency and consistency: Simple locking when multiple users try to modify the system


# Confidentiality 
Want access control so we chose who can access/change what
DB disclosure: Inference attacks and data mining
Info flow control: What can the users do with the data they accessed? Don't want them to be malicious 

All DBs have DAC model
	Based on admins allowing users
		Accessed to data depends on the user and if they have access or not
		Users given ability to give permissions to others
		Granting and revoking is regulated by admin policy 

The DB access control system is there to determine what access rights (or perms) a user has

Subject is what accesses
Object is the item being accessed
Permissions is what the user is allowed to do

## Access control implementation
SQL does it via db privileges 
	SQL has set of priv on objects (relations) rather than file systems
	9 priv (access rights), can be restricted down to one column of one relation (table)
To start, the owner has sole rights to perform operations (DAC)
To manage rights:
	Grant: Owner grant priv to other users (priv delegation)
	Revoke: Owner can remove


GRANT \<priv> ON \<relation>  TO \<users/authID>
For priv, can specify the column

![[Pasted image 20241030113500.png]]
With grant option allows one to pass it to someone else


Revoke is the same except FROM instead of TO
ONLY the priv given by YOU is revoked, if someone got perm elsewhere, it will not be revoked
CASCADE option: Grants made by person that the prem is getting revoked from is also revoked
RESTRICT option: REVOKE fails if priv passed down to others 
<mark style="background: #FF5582A6;">What happens if default is used</mark>

### Role based access control (RBAC)
You give someone (people) a role and that role has the permissions to do various things
![[Pasted image 20241030113949.png]]
Makes it easier if lots of people

RBAC is semantic because it gives meaning to why someone needs a perm
Centered around roles 
Good flexibility and granularity, more stable and easier to manage (potentially fewer things as well)

NIST's RBAC:
	Users
	Roles
	Permissions
	Sessions

Role: Defines individuals and what permissions they have

Static relations:
	User assignment is user is row and role is col
	Permission assignment is P is row and R is col
Dynamic relations:
	Access requests made during sessions 
	Session involves set of roles p(S) that a user has u(S)

Roles can inherent access rights from other roles, like a tree
Represented in a heirarchy, higher role has all perms of lower role
![[Pasted image 20241030115219.png]]


#### COnstraints
Define relationships among roles/conditions related to roles using constraints

Mutually exclusive roles: User only assigned one role in the set
Cardinality: Maximum number of roles allowed
Prerequisite roles: Can only get role if they have another role 


Access control in:
	Role- perms (what role has what perms), stable
	Role- Role (how roles are related) stable
	User- role (what users have what roles) dynamic
Easier to change user's role than modify the roles
Supports least priv and seperation of duties (mutex)


# Disclosure
DB may have sensitive and nonsensitive data
We want to share non-sensitive data for 3rd party use
	Security goal: Only share NON-SENSITIVE data (but can aggregate sensitive data like instead of salaries, av salary)
	Precision goal: share as much non-sensitive data while protecting sens data
	Goal is perf security and max precision 

DB is either all sens, all nonsens, mix
If mix, then:
	Have all data inherently sensitive
	Declared sensitive, or sensitive with other data

### Sens data disclosure
Exact data: Exact value
Bounds: Range of data (lower and upper bound vals of data)
Negative result: Make queries where sens data may be disclosed 
	Find all salary lower than 150k and then look at users (one of upper/lower)
Existence: Disclosing whether data exists or not could be sensitive 
Probable value: It is possible to infer a value of a certain element

This leads to the inference problem where you can infer sensitive data from nonsensitive data

### Direct attack
Asking query that returns values of senstive fields of few records (like sql injection)
![[Pasted image 20241030121319.png]]

To defend, statstical dbs are used like count, sum, mean, med, mode, min, max, etc
Done to allow statistical use without revealing entries

2 types:
	Pure statistical (Census)
	Regular db with stat access 


### Indirect attack
Infer based on stats
![[Pasted image 20241030121547.png]]

Prevented by DBMS concealing data if few records make up data

### Tracker attack
Bypass by making more queries that generate small results
Divide query into parts where each part returns acceptable size but their overlap is small enough to infer data
![[Pasted image 20241030121744.png]]

