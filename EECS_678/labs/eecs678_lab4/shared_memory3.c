#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <sys/wait.h>
#define STR1 "First String"
#define STR2 "Second String"
#define STR3 "Third String"
#define LEN 100

// So this program creates two blocks of memory: THe first is shared memory via shmget and the other unshared via alloc. 
// THe program then creates a fork. After the fork, the program prints the data at the shared and unshared block from the parent which is just First String
// The program then waits for the child which prints the shared and unshared buffers which is again just the First String. Then, the program copies STR2 into both
// buffers and then returns. This causes the parent to be able to move passed the wait which it then prints the 2 buffers again
// The shared buffer has STR2 in it now since both itself and its child process point to the same block of memory. The unshared buffer that was mallocd still prints 
// First string because they point to two different things.

/* Child process */
int do_child(char *shared, char *unshared)
{
  /* Print original values in the two buffers */
  fprintf(stdout, "shared_buf in child: %s\n", shared);
  fprintf(stdout, "unshared_buf in child: %s\n", unshared);

  /* Update the two buffers with STR2 */
  strcpy(shared, STR2);
  strcpy(unshared, STR2);
  
  return 0;
}

  
int main()
{
  /* identifier for the shared memory segment */
  int segment_id;
  /* pointer to the shared memory segment */
  char *shared_buf;
  /* size of the shared memory segment */
  int size;
  /* pointer to unshared memory segment */
  char *unshared_buf;
  pid_t pid;
  int i, status;

  /* allocate a shared memory segment */
  size = sizeof(char) * LEN;
  segment_id = shmget(IPC_PRIVATE, size, S_IRUSR|S_IWUSR);

  /* attach the shared memory segment */
  shared_buf = (char *) shmat(segment_id, NULL, 0);

  /* allocate the unshared memory array */
  unshared_buf = (char *)malloc(size);
  
  /* initialize shared and unshared regions to INIT*/
  strcpy(shared_buf, STR1);
  strcpy(unshared_buf, STR1);

  fprintf(stdout, "shared_buf before fork: %s\n", shared_buf);
  fprintf(stdout, "unshared_buf before fork: %s\n", unshared_buf);

  /*create a child process */
  if((pid = fork()) == -1){
    fprintf(stderr, "Error in fork\n");
    exit(0);
  }
  else if(pid == 0){
    /* child process */
    do_child(shared_buf, unshared_buf);
    exit(0);
  }
  else{
    /* wait for child process to finish */
    wait(&status);
    
    /* parent process */
    fprintf(stdout, "shared_buf after fork: %s\n", shared_buf);
    fprintf(stdout, "unshared_buf after fork: %s\n", unshared_buf);
 
    /* detach the shared memory segment */
    shmdt(shared_buf);

    /* remove the shared memory segment */
    shmctl(segment_id, IPC_RMID, NULL);

    return 0;
  }
}
