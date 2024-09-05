#ifndef LIST_QUESTIONS_H
#define LIST_QUESTIONS_H

#include "LinkedList.h"
#include <stdbool.h>
/*
 * Problem: Determine if a linked list has a cycle
 *
 * Description:
 * Given the head of a linked list, determine if the list contains a cycle. A cycle occurs if a node can be
 * reached again by continuously following the `next` pointer. Internally, 'pos' is used to denote the index
 * of the node that the tail's next pointer is connected to, indicating a cycle. Note that 'pos' is not
 * accessible or passed as a parameter; it's only used for problem understanding and explanation.
 *
 * Task:
 * Implement a function to check if the given linked list has a cycle. The function should return 'true' if a
 * cycle is present and 'false' otherwise.
 *
 * Prototype:
 * bool hasCycle(struct Node *head);
 */

static bool hasCycle(struct Node *head)
{
    struct Node* slow = head;
    struct Node* fast = head;
    while (true) {

        // CHeck if the element after fast is NULL, if it is, return false as it is not a cycle. Otherwise go to the next. 
        // Do this twice because it is twice as fast as slow
        if (fast->next == NULL) {
            return false;
        }
        fast = fast->next;
        if (fast->next == NULL) {
            return false;
        }
        fast = fast->next;

        // If the element after slow is NULL, it is not a cycle so return false (shouldnt happen but just in case)
        if (slow->next == NULL) {
            return false;
        }
        slow = slow->next;

        // If slow and fast are the same, return true (cycle)
        if (slow == fast) {
            return true;
        }
    }
    // Placeholder, shouldn't reach
	return false;
}

/*
 * Problem: Merge Two Sorted Lists
 *
 * Description:
 * You are given the heads of two sorted linked lists, list1 and list2. Your task is to merge these two
 * lists into one single sorted list. The merged list should be constructed by splicing together the nodes
 * of the first two lists without creating new nodes, but by rearranging the nodes from the given lists.
 *
 * Task:
 * Implement a function that merges two sorted linked lists and returns the head of the newly merged sorted
 * linked list.
 *
 * Prototype:
 * struct Node* mergeTwoLists(struct Node* list1, struct Node* list2);
 *
 * Note:
 * Both list1 and list2 are sorted in non-decreasing order.
 */

static struct Node* mergeLists(struct Node* list1, struct Node* list2)
{
    // Create a head to return and a cur to go through the linked list
    struct Node *head;
    struct Node *cur;
    // Edge case
    if (list1 == NULL) {
        return list2;
    } else if (list2 == NULL) {
        return list1;
    }

    // Get the smallest element of the 2 lists and make that the head, then go to the next node for that
    if (list1->data > list2->data) {
        head = list2;
        list2 = list2->next;
    } else {
        head = list1;
        list1 = list1->next;
    }
    // set the current to the head so we can traverse
    cur = head;
    // Go through both lists until one is null
    while ((list1 != NULL) && (list2 != NULL)) {
        //Get the smaller element and append it, then make the head of the list point to the next element
        if (list1->data > list2->data) {
            cur->next = list2;
            cur = cur->next;
            list2 = list2->next;
        } else {
            cur->next = list1;
            cur = cur->next;
            list1 = list1->next;
        }

    }

    // If there are any elements left, append them too
    if (list1 != NULL) {
        cur->next = list1;
    }
    else {
        cur->next = list2;
    }
    

	return head;
}

#endif
