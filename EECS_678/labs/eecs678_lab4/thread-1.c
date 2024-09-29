#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

// Questions:
// 1. No, the child does not change the values of the global and local variables in the parent process. They are only changed for the child
// The parent is unaffected
// 2. Yes, the changes made in the other thread did indeed alter the global and local values of the parent process
// With threads, the global variables for the parent process and the child thread are the same. So the child changing it changes it for the parent too.
// As for the local variable, the two threads have different stacks but since the function was given a pointer to the parent process' local var, it was able to modify 
// the variable directly. If it was not given a *pointer* to the local var, then it would not have been able to modify it but instead have its own local var.

void *child_fn(void *param);

int global; /* global variable */

int main (int argc, char *argv[])
{
  int local;
  int ret;
  
  pthread_t tid; /* thread identifier */
  pthread_attr_t attr; /* set of thread attributes */
  
  /* update local and global variables */
  global = 100;
  local = 678;

  printf("Original values, global = %d, local = %d\n", global, local);
  
  /* create a new process */
  ret = fork();  
  if(ret == 0){ // Child process
    child_fn(&local);
    exit(0);
  }
  else{ // Parent process

    wait(NULL);

    /* Question 1: Are changes made to the local or global variables by the
       child process reflected in the parent process? Explain. */
    printf("After fork, global = %d, local = %d\n", global, local);
  }
       
  /* get the default thread attributes */
  pthread_attr_init(&attr);
  /* create the thread */
  pthread_create(&tid, &attr, child_fn, &local);
  /* wait for the thread to exit */
  pthread_join(tid, NULL);

  /* Question 2: Are changes made to the local or global variables by the
     child thread reflected in the parent process?
     Separately explain what happens for the local and global variables. */
  printf("After thread, global = %d local = %d\n", global, local);
}

/* Function called by the child process/thread */
void *child_fn(void *param)
{
  int *val = (int*)(param);

  global = 678;
  *val = 100;
}
