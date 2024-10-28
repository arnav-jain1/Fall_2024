# Task 1
![[Pasted image 20241028114006.png]]
In the normal case, it worked properly but when I copied it over, it did not work saying password unchanged with manipulation error
![[Pasted image 20241028114955.png]]
For chsh, similar results, it is unable to unless it is in the right folder
# Task 2
## Question 2
![[Pasted image 20241028105348.png]]
This exports a new var, prints it (output is correct), then unsets and reprints which has no output
## Question 3
There are no differences, this is because env variables are shared across processes since they are derived from the shell
Output can be found under task2/diff.txt  (blank)
Output of child and parent are called child.txt and parent.txt respectively

## Question 4 
Output when null in my_env_null.txt (blank)
When running it with the 3rd parameter being NULL, nothing happens.

Output when environ in my_env_environ.txt
Then when changing the param to environ, all the env vars are printed

This shows that execve allows you to decide whether the envirornment vars for the syscall are inherited from the env vars of the shell or not. This is useful for changing them temporarily



## Question 5
The output of the mysystem program can be seen in mysystem_output.txt
This program just calls the system syscall which calls execl which calls execve. The parameter is /usr/bin/env which prints all the env vars. Since execve was called by system which inherits the env from the shell, the new program eventually ends up inheriting all the enviornment vars from the shell. Thus, the output is all of the env vars that the shell has

# Task 3 
## Q 6
![[Pasted image 20241028113734.png]]
There new var that I created printed as normal
LD_LIBRARY_PATH did not show (surprise, but likely because it is very important) 
PATH was printed and changed as expected which also sort of surprised me because PATH is important and shouldn't be messed with too much


## Q 7
![[Pasted image 20241028120452.png]]
No because it says permission denied and whoami just shows the username not the root so they aren't running as the root

![[Pasted image 20241028120519.png]]
When switching to zsh it actually does end up running as the root since whoami prints the root thus we can actually print shadow