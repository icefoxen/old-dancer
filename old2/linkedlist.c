/* linkedlist.c
 * A Linked List in C.
 * This had better fucking work.
 *
 * Simon Heath
 * 30/6/2003
 */

#include "linkedlist.h"
#include <stdlib.h>
#include <stdio.h>


static LL* getEnd( LL* lst )
{
   if( lst->next == NULL )
   {
      return lst;
   }
   else
   {
      return getEnd( lst->next );
   }
}


void ll_add( LL* lst, void* itm )
{
   LL* end = getEnd( lst );
   LL* new = malloc( sizeof( LL ) );

   end->next = new;
   new->next = NULL;
   new->item = (ll_ptr) itm;
}

void ll_remove( LL* lst, ll_ptr itm )
{
   LL* temp;
   /* Check for end-of-list */
   if( lst->next == NULL )
   {
      return;
   }

   /* If the next node's item matches itm, chop the node out and finish */
   if( lst->next->item == itm )
   {
      temp = lst->next;
      lst->next = lst->next->next;
      free( temp );
      return;
   }
   /* Finally, recurse. */
   ll_remove( lst->next, itm );
      
}


static void removeIndexHelper( LL* lst, int index )
{
   LL* temp;

   /* Y'see, to remove a node we have to be in the node BEFORE it.
      That way we can re-route the necessary pointer.  */
   if( index == 1 )
   {
      temp = lst->next;
      lst->next = lst->next->next;
      free( temp );
   }
   else
   {
      removeIndexHelper( lst->next, index - 1 );
   }
}


void ll_removeIndex( LL* lst, int index )
{
   /* Error-checking, baby.  */
   if( (index > ll_length( lst )) || index <= 0 )
   {
      fprintf( stderr, "ERROR: ll_removeIndex: invalid index: %d\n", index );
      fprintf( stderr, "ll_removeIndex: list length is: %d\n", 
         ll_length( lst ) );
      exit( 1 );
   }
   else
   {
      /* We use a helper so we don't do the error-checking over and over.
         Especially since ll_length() is expensive.  */
      removeIndexHelper( lst, index );
   }
}

static int findHelper( LL* lst, ll_ptr itm, int index )
{
   if( lst->item == itm )
      return index;
   else if( lst->next == NULL )
      return -1;
   else
      return findHelper( lst->next, itm, index + 1 );
}

/* Returns the node-number of an item, or -1 if it doesn't exist */
int ll_find( LL* lst, ll_ptr itm )
{
   return findHelper( lst, itm, 0 );
}

/* Just the same as removeIndexHelper, really, and for the same reasons:
 * to avoid error-checking with each recursion.
 * Sure, I could use iteration, but that's boring.  :-P
 */
static ll_ptr getHelper( LL* lst, int index )
{
   if( index == 0 )
      return lst->item;
   else
      return getHelper( lst->next, index - 1 );
}

/* Returns the item at the given index */
ll_ptr ll_get( LL* lst, int index )
{
   if( (index < 0) || (index > ll_length( lst )) )
   { 
      fprintf( stderr, "ERROR: ll_get: invalid index: %d\n", index );
      fprintf( stderr, "ll_get: list length is: %d\n", 
         ll_length( lst ) );
      exit( 1 );
   }
   else
      return getHelper( lst, index );
}

static int lenHelper( LL* lst, int cnt )
{
   if( lst->next == NULL )
      return cnt;
   else
      return lenHelper( lst->next, cnt + 1 );
}
int ll_length( LL* lst )
{
   return lenHelper( lst, 1 );
}

void ll_free( LL* lst )
{
   if( lst->next == NULL )
      free( lst );
   else
   {
      ll_free( lst->next );
      free( lst );
   }
}

void ll_printNode( LL* lst )
{
   printf( "Node address: %x item pointer: %x next pointer: %x\n",
      (int) lst, (int) lst->item, (int) lst->next );
}

/* Makes life nicer by numbering the elements */
static void printListHelper( LL* lst, int index )
{
   if( lst->next == NULL )
   {
      printf( "%03d: Node address: %x item pointer: %x next pointer: %x\n",
         index, (int) lst, (int) lst->item, (int) lst->next );
   }
   else
   {
      printf( "%03d: Node address: %x item pointer: %x next pointer: %x\n",
         index, (int) lst, (int) lst->item, (int) lst->next );
      printListHelper( lst->next, index + 1 );
   }
}

void ll_printList( LL* lst )
{
   printListHelper( lst, 0 );
}

void ll_concat( LL* lst1, LL* lst2 )
{
   LL* tmp = getEnd( lst1 );
   tmp->next = lst2;
}

/* BUNCHES of debugging/test stuff.  Yay!  */
/*
int main()
{
   LL *a;
   LL *b;
   a = malloc( sizeof( LL ) );
   a->item = (ll_ptr) 10;
   a->next = NULL;

   ll_printList( a );

   printf( "Adding...\n" );
   ll_add( a, 20 );
   ll_add( a, 30 );
   ll_add( a, 40 );
   ll_add( a, 50 );
   ll_add( a, 60 );
   ll_add( a, 70 );
   ll_add( a, 80 );
   ll_add( a, 90 );
   ll_add( a, 100 );

   ll_printList( a );

   printf( "Length: %d\n", ll_length( a ) );

   printf( "Removing 0x1e...\n" );
   ll_remove( a, 30 ); 
   ll_printList( a );
   
   printf( "Removing 5th node...\n" );
   ll_removeIndex( a, 5 );
   ll_printList( a );

   printf( "Item 0x3c is at place %d\n", ll_find( a, 0x3c ) );

   printf( "The 5th item is: %x\n", (int) ll_get( a, 5 ) );

   b = malloc( sizeof( LL ) );
   b->item = (ll_ptr) 100;
   b->next = NULL;

   ll_add( b, 200 );
   ll_add( b, 300 );
   ll_add( b, 400 );
   ll_add( b, 500 );
   printf( "Concatenating...\n" );
   ll_concat( a, b );
   ll_printList( a );




   printf( "Freeing...\n" );
   ll_free( a );
   printf( "Done!  W00t!!!\n" );
   return 0;
}
*/
