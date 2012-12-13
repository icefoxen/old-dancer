; dancer-parser.lisp
; Okay, this is the parser for Dancer.
; It takes a whole bunch of symbols outputted by the lexer, and recognizes
; valid sequences of them which it turns into statements.
; Statements are structures stuck together in a tree-form, which get stuck
; together into directives, which tell the compiler what to do with them.
; Directives get fed to the actual backend which turns them into assembly.
;
; It checks that the syntax is valid, but it does NOT yet check that all the
; packages and methods and classes and such are correct.
; It might be handy to build the symbol-table thang here...
; ...Grar.  There are some weird issues with that.  For instance, it IS
; possible to know what symbols and such exist in which package at 
; compile-time, and thus know whether a variable or message call is valid.
; However, the whole point of this language is to be DYNAMICALLY typed and
; LATE bound, so that you can do lots of neat little evaluation-tricks...
;
; Hrm.  Next time I'm using freakin' yacc.
; 
;
; Simon Heath
; 1/11/2003-X/X/2004


; Anyhoo, here's the wossnames.
; I'm using a v as a check-mark meaning "done"
;v identifier      = identifier :: identifier 
;                   | any string that isn't matched as something else.
;v value           = num | chr |  strliteral | symbolliteral | block | list
;v methodcall      = [(] identifier expression expression* [)]
;v expression      = methodcall | value | identifier
;v block           = [ paramdecl statementlist ]
;v var             = VAR [( [identifier] [, identifier] )] [= expression]
;v varlist         = var [, var]*
;v asm             = ASM strliteral
;v statement       = expression | var | asm
;v statementseq    = statement [, statement]*
;
;v arg             = [symbolliteral | symbolliteral = value
;                   | symbolliteral = [||] ]
;v arglist         = ( [paramitem [, paramitem]*] )
;
; inheritancelist = (identifier identifier*)
;
;v namelist        = [| identifier+ |]
; directive       = importbody | usebody | exportbody | tracebody 
;                   | globalbody |constantbody | mixinbody | classbody 
;                   | methodbody | method+body
;v importbody      = IMPORT identifier [namelist] PERIOD
; usebody         = USE identifier [namelist] PERIOD
; exportbody      = EXPORT identifier [namelist] PERIOD
; tracebody       = TRACE identifier PERIOD
; globalbody      = GLOBAL identifier PERIOD
; constantbody    = CONSTANT identifier = value PERIOD
; mixinbody       = MIXIN identifier PERIOD
; classbody       = CLASS identifier inheritancelist [strliteral] 
;                   [varlist] PERIOD
; methodbody      = METHOD identifier paramdecl [strliteral] 
;                   [statementseq] PERIOD
; method+body     = METHOD+ identifier paramdecl [strliteral] 
;                   [statementseq] PERIOD
;
;v list            = [| expression* |]




(load "dancer-lexer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   TYPES   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Identifier
(defstruct identifier name package)

; class
(defstruct classdecl name inherits mixinlist docstring classvars methods)

; instance method
(defstruct methoddecl name arglist docstring vars body)

; class method
(defstruct method+decl name class arglist docstring vars body)

; variable decleration
(defstruct vardecl name initval setter getter)

; constant
(defstruct constdecl name initval)

; global
(defstruct globaldecl name initval)

; trace decleration
(defstruct tracedecl name)

; mixin
(defstruct mixindecl name methodlist)

; module
; Hrm...  This is a link-time construct; I shouldn't have it here.
;(defstruct module name imports exports globals constants traces classes)

; arglist and arg item
(defstruct arglist contents)
(defstruct argitem name initval) ; initval can be nil, value, or :list

; block
(defstruct dcrblock args body)


; list
(defstruct dcrlist contents)

; asm decleration
(defstruct asmdecl contents)

; import decleration
(defstruct importdecl name lst)
(defstruct usedecl name lst)


; A method call
(defstruct methodcall name obj args)

(defstruct dcrclass vars methods)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   CONSTANTS & GLOBALS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This contains the current line, for debugging info.
; it's incremented every time the parser runs across a :newline token.
(defvar *current-line* 0)

; This is what everything is output to.
(defvar *statement-list* ())

; This is where trace-lists go
(defvar *trace-list* ())

; This is where the tokens are read from.  Set it to whatever;
; in dancer.lisp it's set with
; (setf *input-list* (dancer-lex "filename"))
(defvar *input-list* ())

; Hmm...  I suppose I'll just hang on to the modules specified by
; "import" and "use".  Can't really do anything with 'em right now.
; That's a linker issue.
(defvar *import-list* ())

(defvar *use-list* ())

(defvar *current-class* (make-dcrclass))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   FUNCTIONS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Erf...  How the hell do I keep line numbers and such in here?  They don't
; exist anymore!  Instead say which function/directive they're in?  Hmm...
; Yesh, that seems like it could work.
; Erm...  Hmm.
; Okay.  So we leave newlines in the representation, and just pass it along
; to here.
(defmacro parser-error (str &rest args)
   (append 
      `(error 
         (concatenate 'string "Parser error at line ~A~%" ,str)
	 *current-line*)
      args))


; Pushes a new statement onto *statement-list*
(defun push-statement (s)
   (push s *statement-list*))

; Returns the next symbol from *input-list*, treating it as a stream.
(defun next-sym ()
  (if (endp *input-list*)
      ()
    (let ((sym (car *input-list*)))
      ; If we run into a :newline token, we want to skip to the
      ; NEXT token and return it.
      (if (equal sym :newline)
	  (progn
	    (incf *current-line*)
	    (setf *input-list* (cdr *input-list*))
	    (next-sym))
	(progn
	  (setf *input-list* (cdr *input-list*))
	  sym)))))

; Pretty much as it says; returns the next char in *symbol-list* w/o
; changing the stream.
(defun peek-sym ()
   (if (< (length *symbol-list*) 2)
      ()
      (car *symbol-list*)))

; Returns an arbitrary symbol.  This is sometimes necessary.
(defun nth-sym (n)
   (if (< (length *symbol-list*) n)
      ()
      (nth n *symbol-list*)))



; This is the beginning of the finite-automata graph for the parser.
; This is where the Real Work begins.
(defun parse-tokens ()
  (let ((x (next-sym)))
    (cond x
	  ((literal-p x) ())
	  ((chr-p x)     ())
	  ((str-p x)     ())
	  ((num-p x)     ())
	  ((dirc-p x)    ())
	  ((stringp x)   ()))
    (case x
	  (:lparen     ())
	  (:rparen     ())
	  (:period     ())
	  (:comma      ())
	  (:block-start ())
	  (:block-end   ())
	  (:assignment  ())
	  (:package-ref ())
	  (:swap        ())
	  (:list-start  ())
	  (:list-end    ())
	  (()        'done)
	  (t            ()))))


; Okay, now we build all the bits and pieces of the above.
; Actually...  We should just be able to do parse-directive...
; since all code is going to be inside directives...


(defun parse-dirc (dir)
  (let ((dirc-type (dirc-contents dir)))
    (case dirc-type  
	  (:import   (parse-import))
	  (:export   (parse-export))
	  (:trace    (parse-trace))
	  (:use      (parse-use))
	  (:global   (parse-global))
	  (:constant (parse-constant))
	  (:mixin    (parse-mixin))
	  (:class    (parse-class))
	  (:method   (parse-method))
	  (:method+  (parse-method+)))))


(defun parse-period ()
  (if (equal (peek-sym) :period)
    (get-sym)
    (parser-error "Period expected.")))

(defun parse-comma ()
  (if (equal (peek-sym) :comma)
    (get-sym)
    (parser-error "Comma expected.")))


; Parse an identifier of the form foo or foo::bar and return it 
; as a struct.
(defun parse-identifier ()
  (if (stringp (peek-sym))
    (let (x (get-sym))
      ; Check for package ref...
      (if (equal (peek-sym) :package-ref)
	(if (stringp (nth-sym 1))
	  (make-identifier :name (progn (get-sym) (get-sym))
			     :package x)
	  (parser-error "Bad package ref: ~A" (nth-sym 1)))
	(make-identifier :name x :package nil)))
    (parser-error "Bad identifier: ~A" (get-sym))))

; Parses a value, ie a number, char, string, symbol, or block.
; A built-in type of data (though no data in Dancer is truely
; atomic or built-in, being wholly OO...)
(defun parse-value ()
  (let ((nxt (peek-sym)))
    (cond
     ((num-p nxt)              (get-sym))
     ((literal-p nxt)          (get-sym))
     ((str-p nxt)              (get-sym))
     ((chr-p nxt)              (get-sym))
     ((equal nxt :list-start)  (get-sym) (parse-list))
     ((equal nxt :block-start) (get-sym) (parse-block))
     (t (parser-error 
	 "Invalid value:~A~%Number, symbol, string, char or block expected."
	 (get-sym))))))

(defun parse-arg ()
  (let ((nxt (get-sym)))
    (if (equal "=" (peek-sym))
      (progn
	(get-sym)
	(if (and (equal (peek-sym) :list-start)
		 (equal (nth-sym 1) :list-end))
	  (make-argitem :name nxt :initval :list)
	  (make-argitem :name nxt :initval (parse-value))))
      (make-argitem :name nxt :initval nil))))

(defun parse-arglist ()
  (if (equal :rparen (peek-sym))
    (make-arglist :contents nil)
    (make-arglist :contents (parse-arglist-helper ()))))

(defun parse-arglist-helper (lst)
  (let ((nxt (get-sym)))
    (cond
     ; We build the list in reverse order for efficiency
     ((equal nxt :rparen) (reverse lst))
     ((equal nxt :comma)
      (let ((nxt (peek-sym)))
	(if (equal nxt :anyvar)
	  (parse-arglist-helper (cons :anyvar lst))
	  (parse-arglist-helper (cons (parse-arg) lst)))))
     (t (parser-error "Comma expected in arglist!")))))


(defun parse-block ()
  (get-sym)
  (make-dcrblock 
   :args (parse-arglist)
   :body (parse-statementseq)))


; Parses an expression, ie something that can be evaluated
; and return a value.
; It decides whether an input is a value, or a methodcall/identifier
; Complex operations are easy, if you make sufficient helper functions.
; Break things apart to the simplest solvable problem, then build complex
; solutions out of simple ones.
; Gods I love programming.
(defun parse-expression ()
  (let ((x (peek-sym)))
    (if (or (stringp x) (equal x :rparen))
      (parse-exp-helper)
      (parse-value))))

; This is kinda tricky; it decides whether an expression starting
; with an identifier is a variable or a method call.
(defun parse-exp-helper ()
  (let ((x (peek-sym)))
    (if (equal :rparen x)
      (parse-method-call)
      (if (stringp x)
	(if (or (stringp (nth-sym 1)) (equal :rparen (nth-sym 1)))
	  (parse-expression)
	  (parse-identifier))
	(parser-error "Identifier expected: found ~A~%" x)))))
	

; Snaffles a method call of the form "foo bar bop*" or "(foo bar bop*)"
; It stops when it reaches an appropriate rparen, period or comma.
(defun parse-method-call ()
  (if (equal :lparen (peek-sym))
    (get-sym))
  (let* ((nm (try-to-get-string))
	 (obj (try-to-get-string))
	 (args (parse-expression-seq ())))
    (make-methodcall
     :name nm
     :obj obj
     :args args)))
  
(defun parse-expression-seq (lst)
  (if (or 
       (equal :comma (peek-sym))
       (equal :period (peek-sym))
       (equal :rparen (peek-sym)))
    (reverse lst)
    (parse-expression-seq (cons (parse-expression) lst))))
  
  
    
; Parse an asm decleration
(defun parse-asm ()
  (let ((x (get-sym)))
    (if (stringp? x)
      (make-asmdecl :contents x)
      (parser-error "Asm decleration expected"))))

(defun try-to-get-string ()
  (if (stringp? (peek-sym))
    (get-sym)
    (parser-error "Identifier expected")))
      


; Parse a var decleration of the form of:
; var x (x-,x=) = 0
; It expects the "var" to already be slurped.
; Yes yes, lots of lets.  Gimme a break; I've been hacking Ocaml.
; This function is utterly twisted.  Hell, it even LOOKS weird.
; I dunno why.  My mind is odd today, I guess.
(defun parse-var ()
  (let ((nm (try-to-get-string)))
    (let ((gt
	   (if (equal :lparen (peek-sym))
	     (progn
	       (get-chr)
	       (if (equal :comma (peek-sym))
		 ()
		 (try-to-get-string)))
	     ())))
      (let ((st
	     (if (equal :rparen (peek-sym))
	       ()
	       (progn 
		 (try-to-get-string)
		 (get-sym)))))
	(let ((inval
	       (if (equal "=" (get-sym))
		 (parse-value))))
	  (make-vardecl :name nm :initval inval :setter st :getter gt))))))

(defun parse-statement ()
  (cond
   ((equal :asm (peek-sym))
    (get-sym)
    (parse-asm))
   ((equal :var (peek-sym))
    (get-sym)
    (parse-var))
   (t
    (parse-expression))))


; Parses a bunch of comma-seperated statements until it hits a period.
(defun parse-statementseq ()
  (parse-statementseq-helper ()))

; A statement sequence may also be part of a block...
(defun parse-statementseq-helper (lst)
  (if (or (equal :period (peek-sym)) (equal :block-end (peek-sym))
    (reverse lst)
    (progn
      (parse-comma)
      (parse-statementseq-helper (cons (parse-statement) lst)))))

    
  


; Parse a list of identifiers and pass it back.
; A name-list is a SYNTACTIC structure, not a data structure.
; It's used in export lists and such.
(defun parse-namelist (&key (accm ()))
  (if (equal (peek-sym) :list-end)
    accm
    (parse-namelist :accm (cons (parse-identifier) accm))))

; Parses a list of the form [| foo bar bop |] and returns it.
; Lists may be defined in terms of expressions....???
; XXX: Hmm, that works well with interprated languages, but it seems
; kinda hard to compile...
; Well, leave it for now.  It can be done; Ocaml and such proves that.
; I just need to figure out how...  o_o
(defun parse-list (&key (accm ()))
  (if (equal (peek-sym) :list-end)
    (make-dcrlist :contents accm)
    (parse-list :accm (cons (parse-expression) accm))))

(defun parse-inheritancelist ())



(defun parse-import ()
  (if (not (stringp (peek-sym)))
    (parser-error "Invalid import: ~A" (get-sym))
    (progn
      (make-importdecl :name (get-sym) :lst (parse-namelist))
      (parse-period))))

(defun parse-use ())
(defun parse-export ())
(defun parse-trace ())
(defun parse-global ())
(defun parse-constant ())
(defun parse-mixin ())
(defun parse-class ())
(defun parse-method ())
(defun parse-method+ ())