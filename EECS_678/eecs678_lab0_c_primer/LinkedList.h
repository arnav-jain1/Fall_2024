#ifndef LINKED_LIST_H
#define LINKED_LIST_H

// Libraries
#include <stdio.h>
#include <stdlib.h>

/*
 * Linked List Node Definition:
 * In C, structs serve a similar purpose to classes in other languages like
 * Python, but without methods or inheritance. Structs are user-defined data
 * types that allow you to combine primary data types of different kinds.
 *
 * Structs are particularly useful when you need to group data that belongs
 * together, making the code more modular and understandable. For example, a
 * linked list node contains data and a pointer to the next node, forming a
 * chain.
 *
 * Here's how you define a struct for a linked list node:
 * - `int data`: holds the integer data for this node.
 * - `struct Node* next`: is a pointer to the next node in the list, or NULL if
 * there is no next node.
 *
 * This structure is fundamental in creating linked lists in languages like C
 * where high-level abstractions such as Python's list class are not built-in.
 */

struct Node {
  int data;          // Data stored in the node
  struct Node *next; // Pointer to the next node in the list
};

/*
 * Function: createNode
 * Description: Creates a new node for a linked list containing integer data and
 * a pointer to the next node.
 *
 * Parameters:
 *   data - The integer data to store in the node.
 *
 * Return:
 *   A pointer to the newly created node of type 'struct Node'.
 *
 * Note:
 *   - Use 'malloc' to allocate memory for the new node.
 *   - Ensure to define the type of each variable and the function return value.
 */

static struct Node *createNode(int data) {
  struct Node *node = (struct Node *)malloc(sizeof(struct Node));
  if (node == NULL) {
    fprintf(stderr, "Failed to allocate memory");
    return NULL;
  }
  node->data = data;
  node->next = NULL;

  return node;
}

/*
 * Function: insertAtEnd
 * Description: Appends a node with the given data to the end of the linked
 * list.
 *
 * Parameters:
 *   head - A pointer to the head of the list.
 *   data - The integer data to be added to the list.
 *
 * Return:
 *   A pointer to the head of the list.
 *
 * Note:
 *   - If the list is empty (head is NULL), the new node becomes the head.
 *   - Otherwise, traverse to the last node and link the new node.
 */

static struct Node *insertAtEnd(struct Node *head, int data) {
  struct Node *node = createNode(data);
  if (node == NULL) {
    return head;
  }

  if (head == NULL) {
    return node;
  }

  struct Node *cur = head;
  while (cur->next != NULL) {
    cur = cur->next;
  }
  cur->next = node;
  return head;
  // Step 1: Create a new node using 'createNode'. Handle NULL case if 'malloc'
  // fails in 'createNode'. Hint: Use the function 'createNode(int)' to allocate
  // and initialize the new node. Your Code Here

  // Step 2: Check if the list is empty.
  // Hint: If head is NULL, assign the new node to head.
  // Your Code Here

  // Step 3: If the list is not empty, traverse to the last node.
  // Hint: Use a loop to find the last node (where next is NULL).
  // Your Code Here

  // Step 4: Insert the new node at the end of the list.
  // Hint: Set the 'next' of the last node to the new node.
  // Your Code Here

  // Return the head of the list.
  // Your Code Here

  // Placeholder return statement to make the function compile without filling
  // in the details
}

/*
 * Function: printList
 * Description: Prints all the elements in the linked list.
 *
 * Parameters:
 *   head - A pointer to the head of the list.
 *
 * Note:
 *   - Traverse the list from the head and print the data in each node.
 *   - Use a loop to go through each node until the end of the list is reached.
 */

static void printList(struct Node *head) {
  if (head == NULL) {
    printf("List is empty");
    return;
  }
  struct Node *cur_node = head;
  while (cur_node != NULL) {
    printf("%d ", cur_node->data);
    cur_node = cur_node->next;
  }
  printf("\n");
  // Step 1: Check if the list is empty.
  // Hint: If head is NULL, print "The list is empty."
  // Your Code Here

  // Step 2: Traverse the list starting from the head.
  // Hint: Use a while loop; continue until the current node is NULL.
  // Your Code Here

  // Step 3: Print the data of each node.
  // Hint: Access the data of the node using the arrow operator (->).
  // Your Code Here

  // Step 4: Move to the next node.
  // Hint: Update the current node pointer to the next node in the list.
  // Your Code Here
}

/*
 * Function: deleteList
 * Description: Frees all the nodes in the linked list to prevent memory leaks.
 *
 * Parameters:
 *   head - A pointer to the head of the list which will be set to NULL after
 * deletion.
 *
 * Note:
 *   - It's important to free the nodes to release the allocated memory, a key
 * responsibility in C.
 *   - Use a loop to traverse the list, freeing each node one at a time.
 */

static void deleteList(struct Node **head) {
  struct Node *cur = *head;
  struct Node *temp;
  while (cur != NULL) {
    temp = cur->next;
    free(cur);
    cur = temp;
  }
  *head = NULL;
  // Step 1: Declare a pointer to hold the current node and a temporary node.
  // Hint: Use two pointers, one for the current node and another to hold the
  // next node temporarily. Your Code Here

  // Step 2: Traverse the list and free each node.
  // Hint: Use a loop. For each node, before freeing it, save the reference to
  // the next node. Your Code Here

  // Step 3: After all nodes are freed, set the head to NULL to indicate the
  // list is empty. Hint: It is crucial to reset the head pointer to NULL after
  // freeing the nodes to avoid dangling pointers. Your Code Here
}

#endif
