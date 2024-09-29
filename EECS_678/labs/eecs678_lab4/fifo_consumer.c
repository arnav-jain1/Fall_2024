#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#define FIFO_NAME "file.fifo"
#define MAX_LENGTH 1000


// Answers to questions:
// 1. If there is only a producer, then the program waits until there is a consumer
// 2. If there is only a consumer, then the program waits till there is a producer
// 3. If there were more consumers than producers, then the one consuming took turns getting the message. Program a 
// would get the first then program b would get the second then program a would get the third, etc.
// 4. If there are no more consumers, then the program ends after you try and send another message
// 5. If the producer ends before the consumer does, the consumers print that they read 0 bytes and the program ends.

int main()
{
  char str[MAX_LENGTH];
  int num, fd;

  /* create a FIFO special file with name FIFO_NAME */
  mkfifo(FIFO_NAME, 0666);


  /* open the FIFO file for reading. open() blocks for writers. */
  fd = open(FIFO_NAME, O_RDONLY);
  printf("waiting for writers...");
  fflush(stdout);
  
  printf("got a writer !\n");

  do{
    if((num = read(fd, str, MAX_LENGTH)) == -1)
      perror("read");
    else{
      str[num] = '\0';
      printf("consumer: read %d bytes\n", num);
      printf("%s", str);
    }
  }while(num > 0);
}
