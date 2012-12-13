/* vararray.c
 * A variable-length array/stack that resizes automatically.
 *
 * Implementation, yay!
 *
 * Simon Heath
 * 19/9/2003
 */

#include <stdlib.h>
#include <stdio.h>
#include "vararray.h"

#define PTR_SIZE (sizeof( va_ptr ))
#define STACK_TOP (v->index * PTR_SIZE)


/* Allocates and initializes a new array */
/* Returns 1 on success, 0 on failure.  */
int va_init( va_vararray* v )
{
   va_ptr* x;
   v->size = VA_INITIAL_SIZE;
   v->index = 0;
   if( (int) (x = (va_ptr*) malloc( (VA_INITIAL_SIZE * PTR_SIZE ))) > 0 )
   {
      printf( "Malloc'd: %X\n", v->contents );
      *v->contents = x;
   }
   else
   {
      fprintf( stderr, "va_init: ERROR: Out of memory!!\n" );
      return 0;
   }
   return 1;
}


/* Copies all the values from one array to another one.  */
/* Does no error checking, 'cause it's only EVER called by va_expand.
 * It should always get a valid argument, then.  */
static void copy( va_ptr *v1, va_ptr *v2, int n1, int n2 )
{
   int i;
   for( i = 0; i < n1; i++ )
   {
      /* Must do some casting nonsense here to get it to treat 'em as
       * pointers. */
      *((int*) v2) = *((int*) v1);
      v1 += sizeof( int );
      v2 += sizeof( int );
   }
}

/* Increases the size of a full array.  */
/* Kills the program if it fails.  */
/* You don't want this one to fail.  */
void va_expand( va_vararray *v )
{
   va_ptr old = v->contents;
   va_ptr newe;

   /* We reset the size right off 'cause it'll be useful.  */
   v->size *= VA_RESIZE_AMT;

   printf( "Reallocating memory\n" );
   realloc( v, (v->size * PTR_SIZE ) );
   printf( "Dun.\n" );
   fflush( stdout );

/*
   if( (newe = (va_ptr) malloc( (v->size) * PTR_SIZE)) < 0 )
   {
      fprintf( stderr, "va_expand: ERROR: Out of memory!!\n" );
      exit( 1 );
   }
*/

   /*
   printf( "Copying %X: %d elements to %X\n", old, (v->size/VA_RESIZE_AMT),
   newe );
   fflush( stdout );
   */
   /* Copy the values from the old array to the new one.  */
 /*  copy( old, newe, (v->size / VA_RESIZE_AMT), v->size ); */

   /* Free the old one and set the contents to the new one.  */
   /* WHY the fuck does free() create a segfault??? */
   /*
   printf( "replacing %X %X with %X %X\n", old, *((int*) old), newe, *((int*) newe) );
   free( (void*) old );
   v->contents = (void*) newe;
   */
}


/* Returns the size of an array  */
int va_size( va_vararray *v )
{
   return v->size;
}

/* Returns the number of items in an array  */
int va_index( va_vararray *v )
{
   return v->index;
}

/* Pushes a new item to the top of an array  */
/* Returns 0 if the array isn't full, or 1 if it is.  */
int va_push( va_vararray *v, va_ptr item )
{
   int ret = 0;
   /* Resize if necessary...  */
   if( (v->index >= v->size) )
   {
      printf( "Expanding...\n" );
      fflush( stdout );
      va_expand( v );
      ret = 1;
   }

   /* Ick... nested pointers to pointers with items passed as pointers... */
   /* What we're doing is grabbing the contents, doing some pointer arithmatic
    * to it to get to the top of the stack, and setting it to point to the
    * item  */
   *(v->contents + (STACK_TOP)) = (va_ptr*) item;
   v->index++;


   return ret;
}

/* Pops and returns an item.  */
va_ptr va_pop( va_vararray *v )
{
   v->index--;
   return *((va_ptr*) v->contents + (STACK_TOP));
}

/* Pushes an array of items.  */
void va_pushs( va_vararray *v, va_ptr itemlist, int itemnum )
{
   int i;
   for( i = 0; i < itemnum; i++ )
   {
      va_push( v, (itemlist + (i * PTR_SIZE)) );
   }
}

/* Drops an item from the top of the stack */
void va_drop( va_vararray *v )
{
   v->index--;
}

/* Drops a bunch of items from the top of the stack */
void va_drops( va_vararray *v, int count )
{
   v->index -= count;
}

/* Sticks an element into the array */
void va_put( va_vararray *v, va_ptr item, int place )
{
   fprintf( stderr,
      "va_put: warning: Resizing array from length %d to at least length %d\n",
      v->size, place );

   while( v->size < place )
      va_expand( v );

   *(v->contents + (place * PTR_SIZE)) = (va_ptr*) item;
}

/* Grabs an element from the array and returns it */
va_ptr va_get( va_vararray *v, int place )
{
   if( place > v->size )
   {
      fprintf( stderr,
         "va_get: ERROR: Trying to get element %d, max length is %d\n",
	 place, v->size );
      exit( 1 );
   }
   return (va_ptr) *(v->contents + (place * PTR_SIZE));
}

/* Frees the array */
void va_free( va_vararray *v )
{
   free( (void*) (v->contents) );
   return;
}

void va_freeall( va_vararray *v )
{
   int i;
   for( i = 0; i < v->size; v++ )
   {
      free( *(v->contents + (i * PTR_SIZE)) );
   }
   va_free( v );
}

void va_print( va_vararray *v )
{
   printf( "Size: %d  Index: %d  Addr: %0X\n", v->size, v->index, v->contents);
}
