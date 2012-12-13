/* Write some code, compile it to asm, and insert random
 * cmp %eax, %ebx; jz foo;'s!!!
 */

#include <stdio.h>

int main()
{
   int x = 0;
   printf( "%d\n", x );
   printf( "%d\n", x += 3 );
   printf( "%d\n", 7 + x );
   printf( "%d\n", x -= 7 * x );
   printf( "%d\n", x-- );
   printf( "%d\n", --x );
   return 0;
}

void foo( char *c )
{
   printf( "%s\n", c );
}
