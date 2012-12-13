

#include <stdlib.h>
#include <stdio.h>
#include "vararray.h"


int main()
{
   va_vararray* v;
   int f;
   int i;
   va_init( v );
   //va_free( v );
   //exit( 0 );
   va_print( v );

   va_push( v, (va_ptr) 10 );
   va_push( v, (va_ptr) 'c' );
   va_push( v, (va_ptr) "SPOOOOON!" );
   printf( "Stuff pushed.\n" );
   va_print( v );
   fflush( stdout );

   f = (int) va_pop( v );
   printf( "%s\n", f );
   f = (int) va_pop( v );
   printf( "%c\n", f );
   f = (int) va_pop( v );
   printf( "%d\n", f );
   printf( "Stuff popped.\n" );
   va_print( v );
   fflush( stdout );

   printf( "Cranking up...\n" );
   for( i = 0; i < 100; i++ )
   {
      va_print( v );
      printf( "Pushing: %d\n", i );
      va_push( v, (va_ptr) i );
      printf( "Pushed.\n" );
      fflush( stdout );
   }
   va_print( v );
   printf( "Cranking down...\n" );
   for( i = 0; i < 100; i++ )
   {
      va_pop( v );
   }
   va_print( v );
   fflush( stdout );

   // Just.... wrong, wrong wrong...
   // It works if I free it right after init'ing it, but not later...
   // And it segfaults on the "return 0"...  GAH.
   printf( "Freeing...\n" );
   fflush( stdout );
   va_free( v );
   printf( "Freed.\n" );
   fflush( stdout );

   return 0;
}
