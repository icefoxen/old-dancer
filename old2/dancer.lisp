; Just a few functions to make life easier
; Once I finish everything, this'll contain the actual interface.
; Compiler driver, y'know.

(defpackage :dancer)
(in-package :dancer)
(use-package '(common-lisp ext))

(defparameter *tmp* ())

(defun ld ()
   (load "dancer-parser.lisp")          ; Imports dancer-lexer
   (setf *input-list* (dancer-lex "foo")))
   
(ld)
