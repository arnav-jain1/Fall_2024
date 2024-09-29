/*
  pipe-sync.c: Use Pipe for Process Synchronization
*/

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>

// Question: Update this program to synchronize execution of the parent and
// child processes using pipes, so that we are guaranteed the following
// sequence of print messages:
// Child line 1
// Parent line 1
// Child line 2
// Parent line 2


int main()
{
  char *s, buff[1024];
  bzero(buff, 1024);
  int ret, stat;
  s  = "Use Pipe for Process Synchronization\n";

  /* create pipe */
  int fd1[2];
  int fd2[2];
  pipe(fd1);
  pipe(fd2);

  ret = fork();
  if (ret == 0) {
    close(fd1[0]);
    close(fd2[1]);

    printf("Child line 1\n");
    write(fd1[1], buff, 1024);

    /* child process. */
    read(fd2[0], buff, 1024);
    printf("Child line 2\n");
    write(fd1[1], buff, 1024);

    close(fd1[1]);
    close(fd2[0]);
  } else {
    close(fd2[0]);
    close(fd1[1]);

    /* parent process */
    read(fd1[0], buff, 1024);
    printf("Parent line 1\n");
    write(fd2[1], buff, 1024);

    read(fd1[0], buff, 1024);
    printf("Parent line 2\n");

    close(fd1[0]);
    close(fd2[1]);

    wait(&stat);
  }
}
