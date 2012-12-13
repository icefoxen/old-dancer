/* vararray.h
 * A variable-length array/stack that resizes automatically.
 *
 * How far it should resize is a bit of an issue.  At the moment, it doubles
 * it's size each time it overflows, but other possibilities would be squaring
 * itself or increasing by 1.5 times.
 *
 * In terms of Dancer, this is mostly for the call stack, so it has alot of
 * stack-ish functions.  But we also need to be able to grab stuff out of the
 * middle, so it's also an array.
 *
 * Okay, the type-system is thus.  These things store a var-length array of
 * pointers.  These pointers can be set to any type.  However, you MUST KNOW
 * what kind of things you're getting in and out, and cast them appropraitely.
 * So if you want to push a string into the thing, you must do:
 * va_push( v, (va_ptr) s );
 * Then to get the string:
 * s = (char*) va_pop( v );
 *
 * Simon Heath
 * 19/9/2003
 */

#ifndef _VARARRAY_H
#define _VARARRAY_H

/* How much it should resize on overflow */
#define VA_RESIZE_AMT 2
#define VA_INITIAL_SIZE 20

/* The SIZE attribute counts from ONE and tells the MAX size of the array.
 * The INDEX attribute counts from ZERO and tells HOW MANY items are in the
 * array.  */

/* This may or may not work...  Defining our own general pointer type.  */
typedef void* va_ptr;

typedef struct {
   int size;
   int index;
   /* Array of pointers, baby.  */
   va_ptr* contents;
} va_vararray;

/* Allocates and initializes a new array */
/* Returns 1 on success, 0 on failure.  */
int va_init( va_vararray *v );

/* Increases the size of a full array.  */
/* Returns the size of it.  */
void va_expand( va_vararray *v );

/* Returns the size of an array  */
int va_size( va_vararray *v );

/* Returns the number of items in an array */
int va_index( va_vararray *v );

/* Pushes a new item to the top of an array  */
/* Returns 0 if the array isn't full, or 1 if it is.  */
int va_push( va_vararray *v, va_ptr item );

/* Pops and returns an item.  */
va_ptr va_pop( va_vararray *v );

/* Pushes an array of items.  */
void va_pushs( va_vararray *v, va_ptr itemlist, int itemnum );

/* Drops an item from the top of the stack */
/* Does NOT free it!!! */
void va_drop( va_vararray *v );

/* Drops a bunch of items from the top of the stack */
/* Does NOT free them!!! */
void va_drops( va_vararray *v, int count );

/* Sticks an element into the array */
void va_put( va_vararray *v, va_ptr item, int place );

/* Grabs an element from the array and returns it */
va_ptr va_get( va_vararray *v, int place );

/* Frees the array */
/* WARNING: IT DOES **NOT** FREE the elements IN the array!!  */
void va_free( va_vararray *v );

/* This one does though.  */
void va_freeall( va_vararray *v );

void va_print( va_vararray *v );

#endif /* _VARARRAY_H */
