/*
 * fork.c: Program to learn about multiple fork calls
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

// Question 1: How many processes are created? Explain.
// 8 total. The first fork creates 2. Then 2 processes hit the second fork so they each create one for a total of 4. Then 
// The 4 processes each hit the last fork creating 1 each for a total of 8

int main(int argc, char *argv[])
{
  pid_t ret1=0, ret2=0, ret3=0;
  int ret_stat, pid;

  fprintf(stdout, "Fork-1 was called from process: %d\n", getpid());
  ret1 = fork();

  fprintf(stdout, "Fork-2 was called from process: %d\n", getpid());
  ret2 = fork();

  fprintf(stdout, "Fork-3 was called from process: %d\n", getpid());
  ret3 = fork();

  return(0);
}
