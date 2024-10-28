# Task 1

# Task 2
## Question 2
![[Pasted image 20241028105348.png]]

## Question 3
There are no differences, this is because env variables are shared across processes since they are derived from the shell

## Question 4 
When running it with the 3rd parameter being NULL, nothing happens.
Then when changing the param to environ, all the env vars are printed

This shows that execve allows you to decide whether the envirornment vars for the syscall are inherited from the env vars of the process or not. This is useful for changing them temporarily



## Question 5
The output of the mysystem program can be seen in mysystem_output.txt
This program just calls the system syscall which calls execl which calls execve. The parameter is /usr/bin/env which prints all the env vars since execve was called by system which inherits the env from the shell. The new program eventually ends up inheriting all the enviornment vars from the shell.


# Task 3 
## Q 6
There new var that I created was the same, LD_LIBRARY_PATH did not show (surprise, likely because it is very important) and PATH did not have my new one (also surprise because it is important)


## Q 7
No because it says permission denied and whoami just shows the username not the root
