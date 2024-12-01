#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define MAXSTR 1000

int main(int argc, char *argv[])
{
  char line[MAXSTR];
  int *page_table, *mem_map;
  unsigned int log_size, phy_size, page_size, d;
  unsigned int num_pages, num_frames;
  unsigned int offset, logical_addr, physical_addr, page_num, frame_num;

  /* Get the memory characteristics from the input file */
  fgets(line, MAXSTR, stdin);
  if((sscanf(line, "Logical address space size: %u^%u", &d, &log_size)) != 2){
    fprintf(stderr, "Unexpected line 1. Abort.\n");
    exit(-1);
  }
  fgets(line, MAXSTR, stdin);
  if((sscanf(line, "Physical address space size: %u^%u", &d, &phy_size)) != 2){
    fprintf(stderr, "Unexpected line 2. Abort.\n");
    exit(-1);
  }
  fgets(line, MAXSTR, stdin);
  if((sscanf(line, "Page size: %u^%u", &d, &page_size)) != 2){
    fprintf(stderr, "Unexpected line 3. Abort.\n");
    exit(-1);
  }

  /* Allocate arrays to hold the page table and memory frames map */
  num_pages = pow(2, log_size - page_size);
  page_table = (int *) malloc(sizeof(int) * num_pages);
  num_frames = pow(2, phy_size - page_size);
  mem_map = (int *) malloc(sizeof(int) * num_frames);
  printf("Number of Pages: %u, Number of Frames: %u\n\n", num_pages, num_frames);

  /* Initialize page table to indicate that no pages are currently mapped to
     physical memory */
  for (unsigned int i = 0; i < num_pages; i++) {
      page_table[i] = -1;
  }
  

  /* Initialize memory map table to indicate no valid frames */
  
  for (unsigned int i = 0; i < num_pages; i++) {
      mem_map[i] = -1;
  }

  /* Read each accessed address from input file. Map the logical address to
     corresponding physical address */
  fgets(line, MAXSTR, stdin);
  int counter = 0;
  while(!(feof(stdin))){
    sscanf(line, "0x%x", &logical_addr);
    fprintf(stdout, "Logical Address: 0x%x\n", logical_addr);
    
	/* Calculate page number and offset from the logical address */

    page_num = logical_addr >> page_size;
    fprintf(stdout, "Page Number: %x\n", page_num);

    frame_num = page_table[page_num];
    if (frame_num == -1) {
        fprintf(stdout, "Page Fault!\n");
        frame_num = counter;

        page_table[page_num] = frame_num;
        mem_map[frame_num] = frame_num;
        counter++;
    }

    fprintf(stdout, "Frame Number: %x\n", frame_num);
    unsigned int mask = (1 << page_size) - 1;
    offset = logical_addr & mask;


    

    /* Form corresponding physical address */

    physical_addr = (frame_num << page_size) | offset;
    
    fprintf(stdout, "Physical Address: 0x%x\n\n", physical_addr);

    /* Read next line */
    fgets(line, MAXSTR, stdin);    
  }

  return 0;
}
