; This tries to see what happens when the CPU stack overflows.
; Fun, ne?
;
; Oh, a segfault.  How original.

global main

_start:
   push 0x10
   call main

main:
   mov ip, main
   call _start
