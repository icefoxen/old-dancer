; dancer-lexer.lisp
; This is, as the name suggests, the lexer for Dancer.
; Yes, I could use lex or ocamllex or any of the zillion clones.
; I'm not going to, because I'm preeeeetty sure I know how to write this,
; and I think it'll be interesting.
; Also, I like knowing exactly what's going on.
;
; And I'm somewhat odd.
;
; Simon Heath
; 26/9/2003
;
; Yay, it's done!  A mere three days later.  Let's work on the parser!
; Erm, waitasec, literals don't work.  Hm.
; SH
; 29/9/2003
;
; Yay!  It's done for real this time!  Or at least I think it does everything
; I want it to do.  I'm sure I'll discover it's very not-done later on in
; the process, but for now it seems to work and be fairly sturdy, so I'll
; move on to the parser.
; SH
; 17/10/2003

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   TYPES   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A symbol-literal
(defstruct literal contents)

; A character
(defstruct chr contents)

; A string
(defstruct str contents)

; A number --for now we only use fixnums and floats, for simplicity's sake.
(defstruct num contents)

; A compiler directive --something like "method" or "class" or such.
(defstruct dirc contents)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   CONSTANTS & GLOBALS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *symbol-list* ())

(defvar *file-contents* ())

(defvar *line-no* 1)
(defvar *char-no* 1)

; Reset these for interactive debugging
(setf *symbol-list* ())
(setf *file-contents* ())
(setf *line-no* 1)
(setf *char-no* 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   FUNCTIONS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro lexer-error (str &rest args)
   (append 
      `(error 
         (concatenate 'string "Lexer error: Line ~A char ~A~%" ,str)
	 *line-no*
	 *char-no*)
      args))

(defun push-symbol (s)
   (if (equal s "")
      ()
      (push s *symbol-list*)))

;; Slurps a whole file into the *file-contents* global var.
;; You would not BELIEVE how silly this can get.
;; Something along the lines of (setf foo (append foo (list (read-char s))))
(defun slurp-file (name)
   (with-open-file (s name)
      (loop do
	 (push (read-char s) *file-contents*)
         until (= (file-position s) (file-length s))))
   (setf *file-contents* (reverse *file-contents*)))

;; Treats the *file-contents* var as a stream, sorta, incrementing
;; the character and line counts as it goes.
(defun next-chr ()
   (if (endp *file-contents*)
      ()
      (let ((chr (car *file-contents*)))
         (case chr
	    (#\newline 
	       (incf *line-no*) 
	       (setf *char-no* 1))
	    (t
	       (incf *char-no*)))
	 (setf *file-contents* (cdr *file-contents*))
	 chr)))

; Pretty much as it says; returns the next char in *file-contents* w/o
; changing the stream.
(defun peek-chr ()
   (if (< (length *file-contents*) 2)
      ()
      (car *file-contents*)))

; Returns an arbitrary char.  This is sometimes necessary.
(defun nth-chr (n)
   (if (< (length *file-contents*) n)
      ()
      (nth n *file-contents*)))
   

; This is the main driver function.  The beginning of the finite-automata
; graph.
; It just matches the beginning of the given list with a variety of characters
; and calls the appropriate function to proceed.
; The handling of termination-cases (ie a null list) is rather wonky though,
; since it can terminate here or in certain of the sub-functions.  Don't ask me
; how it works, please, it's just too big to hold in my mind.
(defun get-tokens ()
  (let ((x (next-chr))) 
      (case x
         (#\( (add-lparen))
         (#\) (add-rparen))
	 (#\newline (add-newline))
         (#\[ (scan-lbracket))
         (#\] (scan-rbracket))
	 (#\| (scan-list-end))
         (#\{ (scan-block-comment))
         (#\; (scan-line-comment))
         (#\. (add-period))
         (#\, (add-comma))
         (#\: (scan-colon))
         (#\' (scan-literal))
         (#\" (scan-string))
         (#\# (scan-char))
         (#\_ (add-anyvar))
         (#\space (get-tokens))
	 (#\tab (get-tokens))
         (#\0 (scan-number (string x))) 
         (#\1 (scan-number (string x)))
         (#\2 (scan-number (string x)))
         (#\3 (scan-number (string x)))
         (#\4 (scan-number (string x)))
         (#\5 (scan-number (string x)))
         (#\6 (scan-number (string x)))
         (#\7 (scan-number (string x)))
         (#\8 (scan-number (string x)))
         (#\9 (scan-number (string x)))
         (#\- (scan-minus))  ; A minus can be part of a symbol OR number.
	 ('() 'done) 
	 ; Okay.  Normally, we slurp the leading char and ignore it,
	 ; since it tells us what TYPE of symbol is happening.  But for
	 ; regular symbols, we pass it on, since it's PART of the symbol
	 ; This happens for scan-number too.
         (t (scan-symbol (string x))))))

; These just add literal symbols to the symbol list.
; Parens are never part of a lexical sequence; they stand alone.
; While building the symbol list, we push all elements to the beginning, for
; efficiency.
; All these functions are mutually-recursive; they all call get-tokens
; when they're done.
(defun add-lparen ()
   (push-symbol :lparen)
   (get-tokens))

(defun add-rparen ()
   (push-symbol :rparen)
   (get-tokens))

(defun add-comma ()
   (push-symbol :comma)
   (get-tokens))

; We want to remember where the newlines are, so the parser can print sane
; debugging messages too.
(defun add-newline ()
   (push-symbol :newline)
   (get-tokens))

; This does opening and closing brackets, and also lists.
; If it goes [| ... |] it's a list, otherwise it goes [ ... ] and it's a block.
(defun scan-lbracket ()
   ; Check for lists...
   (if (char= (peek-chr) #\|)
      (progn
         (next-chr)  ; Slurp the extra character
         (push-symbol :list-start)
	 (get-tokens))
      (progn
         (push-symbol :block-start)
	 (get-tokens))))

; This is at the same time simpler and more complex, since to check for
; list-end we have to look for the | first.
(defun scan-rbracket ()
   (push-symbol :block-end)
   (get-tokens))

; This checks for an ending |].  If it finds a | alone, it signals an error
(defun scan-list-end ()
   (if (char= (peek-chr) #\])
      (progn
         (push-symbol :list-end)
	 (get-tokens))
      (lexer-error "Malformed list.")))


; These functions scan for comments.
; This one just checks for the starting {{ and calls end-block-comment.
; Remember, when these functions have been called, the first character
; has already been slurped by (get-tokens)
(defun scan-block-comment ()
   (if (char= #\{ (peek-chr))
      (end-block-comment)
      (lexer-error "Malformed comment.")))

; This slurps chars until it reaches a newline, then calls get-tokens.
(defun scan-line-comment ()
   (if (char= (next-chr) #\newline)
      (add-newline)
      (scan-line-comment)))

; This slurps chars until it reaches an ending }}, then calls get-tokens
; It also handles nested block comments.
(defun end-block-comment ()
   (cond 
      ((char= #\newline (peek-chr))
         (next-chr)
	 (push-symbol *symbol-list* :newline)
	 (end-block-comment))
      ((char= #\} (next-chr) (peek-chr))
         (next-chr)
         (get-tokens))
      ((char= #\{ (next-chr) (peek-chr))
         (next-chr)
	 (end-block-comment)
	 (end-block-comment))
      (t (end-block-comment))))

; This just adds a period.
(defun add-period ()
   (push-symbol :period)
   (get-tokens))

; Same ol' same ol'.
(defun add-comma ()
   (push-symbol :comma)
   (get-tokens))

; This scans a colon to see whether it's part of a package reference, or an
; assignment/swap statement
(defun scan-colon ()
   (cond
      ; Check for a package-reference...
      ((char= #\: (peek-chr)) 
         (next-chr)
         (push-symbol :package-ref)
         (get-tokens))
      ; Check for an assignment or swap statement...
      ; Must remember to recurse into get-tokens!!!
      ((char= #\= (peek-chr))
         (next-chr)
	 (if (char= #\: (peek-chr))
	    (progn
	       (push-symbol :swap)
	       (next-chr)
	       (get-tokens))
	    (progn
	       (push-symbol :assignment)
	       (get-tokens))))
      ; Else, it's invalid
      (t
         (lexer-error "Colon isn't part of a package or assignment"))))


; Just pushes a literal-struct containing the symbol after the '
; For some reason this didn't work, but after making no changes whatsoever
; it does now.  Scary.
(defun scan-literal ()
   (push-symbol (make-literal :contents (scan-symbol-helper "")))
   (get-tokens))

; Scans a char.  Resolves #nl, #cr, #lf, #sp, and #tb into appropriate chars.
(defun scan-char ()
   (push-symbol (make-chr :contents (scan-char-helper)))
   (get-tokens))

; We use a helper function because we also want to be able to resolve
; chars from inside strings.  So instead of building the char-structure
; and recursing, we just return the char.
(defun scan-char-helper ()
   (let ((x (next-chr)))
      (case x
         (#\n 
	    (if (char= #\l (peek-chr))
	        (progn
		   (next-chr)
		   #\newline)
		#\n))
	 (#\c
	    (if (char= #\r (peek-chr))
	        (progn
		   (next-chr)
		   #\return)
		#\c))
         (#\l
	    (if (char= #\f (peek-chr))
	        (progn
		   (next-chr)
		   #\linefeed)
		#\l))
         (#\s
	    (if (char= #\p (peek-chr))
	        (progn
		   (next-chr)
		   #\space)
		#\s)) 
         (#\t
	    (if (char= #\b (peek-chr))
	        (progn
		   (next-chr)
		   #\tab)
		#\t))
         (t x))))

; Builds a string, scans for char-literals inside it.
(defun scan-string ()
   (scan-string-helper ""))

; Hmmm....  It seems that when it calls scan-char, it never closes the string
; 'cause scan-char zaps right back to get-tokens...
; I dunno how to fix that one w/o re-writing a special scan-char that just
; returns the char... but that could work, 'cause then scan-char itself can
; just call the new one... yeah.
; Yay, it works!  ^_^
; I'm leaving the old ramble in for reference.
(defun scan-string-helper (str)
   (let ((x (next-chr)))
      (case x
         ; If there are character-literals in the string, grab 'em out
         (#\#
	    (scan-string-helper
	       (concatenate 'string str (string (scan-char-helper)))))
         ; Else, we just need to check for ending "'s
	 (#\"
	    (push-symbol (make-str :contents str))
	    ; And recurse...
	    (get-tokens))
	 ; And append new chars
	 (t
	    (scan-string-helper
	       (concatenate 'string str (string x)))))))

 
 ; Adds an anyvar symbol, represented by _
(defun add-anyvar ()
   (push-symbol :anyvar)
   (get-tokens))


; XXX:
; Hmmmmm....
; Lisp doesn't have any pure string-to-arbitrary-number functions; the closest
; it has is (read-from-string), which is bloody dangerous because it will read
; and evaluate ANYTHING from the string.  True, a compiler isn't much of
; a security risk by nature, and this func is isolated from the user,
; but I still don't like it.
; ...really, I'm not sure how to handle numbers here, especially because
; bignums and ratios are above the level of assembly.  I'll just leave 'em
; as strings, I suppose.
; Ah well, let's just recognize the numbers.  Dealing with them comes later.
;
; So split this up into three sub-funcs: scan-decimal, scan-hex and
; scan-binary.
;
; This whole number-parsing is a tad crazy; I'm not completely sure how it
; should work.  I'm sure it'll become obvious later though, so I'll just leave
; this.  I can always rewrite it.
(defun scan-number (n)
   ; Recognize binary and hex numbers, and call the appropriate scanners
   (if (string= n "0")
      (case (peek-chr)
         (#\x
            (scan-hex (concatenate 'string n (string (next-chr)))))
	 (#\b
            (scan-binary (concatenate 'string n (string (next-chr)))))))
	 ;(t
         ;   (scan-octal (concatenate 'string n (string (next-chr)))))))
   (scan-decimal n))
	     
#|
	 ((and (char= s #\0) (char= (nth-chr 1) #\x))
	    (scan-hex
	       (concatenate 'string 
	          n (string (next-chr)) (string (next-chr)))))
	 ((and (char= (elt n 0) #\0) (char= (nth-chr 1) #\b))
	    (scan-binary
	       (concatenate 'string 
	          n (string (next-chr)) (string (next-chr)))))
	 ; Otherwise, it's just a decimal number.
	 ; If s is a number, cat it on the string and recurse
         ((find s "1234567890")
	    (format t "~A ~A~%" s n)
	    (scan-number
	       (concatenate 'string 
	          n (string (next-chr)))))
	 ; If s is a decimal and no decimal is already in the string,
	 ; cat it on and recurse.  Otherwise, error.
	 ((char= s #\.)
	    (if (find #\. n)
	       (lexer-error "Invalid number format")
	       (scan-number
	          (concatenate 'string n (string (next-chr))))))
	 ; Same goes for ratios, form n/m
	 ((char= s #\/)
	    (if (find #\/ n)
	       (lexer-error "Invalid number format")
	       (scan-number
	          (concatenate 'string n (string (next-chr))))))
         ; Anything else isn't part of a number, so we finish up and 
	 ; call get-tokens
	 (t
	    (push-symbol (make-num :contents n))
	    (get-tokens)))))
|#

; Rar... instead of decimal/hex/binary should I divvy it up by integer/float/
; ratio?  That may make more sense from a programming point of view...
; 'Twould also be nice to be able to have ratios represented as hex and
; maybe binary; 'tis definately the Right Thing.  Ah well, later.
(defun scan-decimal (n)
   (let ((s (peek-chr)))
      (if (null s)
         (progn
	    (next-chr)
	    (get-tokens))
      (cond
	 ; If s is a decimal and no decimal is already in the string,
	 ; cat it on and recurse.  Otherwise, error.
         ((and (find (nth-chr 1) "0123456789") (char= #\. s))
	    (if (find #\. n)
	       (lexer-error "Invalid number format")
	       (scan-decimal
	          (concatenate 'string n (string (next-chr))))))
	 ; Same goes for ratios, form n/m
	 ((char= s #\/)
	    (if (find #\/ n)
	       (lexer-error "Invalid number format")
	       (scan-decimal
	          (concatenate 'string n (string (next-chr)))))) 
         ((find s "0123456789")
	    (scan-decimal
	       (concatenate 'string n (string (next-chr)))))
	 (t
	    (push-symbol (make-num :contents n))
	    (get-tokens))))))

(defun scan-hex (n)
   (let ((s (peek-chr)))
      (cond
         ((find s "0123456789ABCDEFabcdef")
	    (scan-hex
	       (concatenate 'string n (string (next-chr)))))
	 (t
	    (push-symbol (make-num :contents n))
	    (get-tokens)))))

(defun scan-binary (n)
   (let ((s (peek-chr)))
      (cond
         ((find s "10")
	    (scan-binary
	       (concatenate 'string n (string (next-chr)))))
	 (t
	    (push-symbol (make-num :contents n))
	    (get-tokens)))))

; Reads a fairly arbitrary symbol.
(defun scan-symbol (s)
   (push-symbol (scan-symbol-helper s))
   (get-tokens))

; This basically reads in any valid symbol and returns it.
; If the next char is a valid character, add it to the current symbol.
; Otherwise, return the thang.
; _ is NOT a valid character!
(defun scan-symbol-helper (s)
   (let ((c (peek-chr)))
      (if (find c
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890*/-+!?<>=")
	 (scan-symbol-helper
	    (concatenate 'string s (string (next-chr))))
	 s)))


; This shuffles through a list and grabs out all the compiler directives.
; It works differently from the previous functions, 'cause it's not part of
; the (get-token) finite-automata tree.  It operates on it's own after
; (get-token) is done.
(defun ident-directives ()
   (ident-directive-helper *symbol-list* ()))

; The neat thing is that this reverses the list given as it parses it...
; So when the list gets built in reverse order by all the (push-token) calls,
; (we append new items to the front of the list instead of the back, so it's
; not utterly rediculiously inefficient) it gets fed through this once it's 
; all done so it puts it in the right order automatically.
; Haha, and I never even designed this; I had just planned to call (reverse)
; on the list when I ended up needing to, but this way works pretty shibbily.
; ^_^
(defun ident-directive-helper (s l)
   (if (null s)
      l
      (let ((i (car s)))
         (if (or
                (equal i "method")
                (equal i "method+")
                (equal i "import")
                (equal i "use")
                (equal i "export")
                (equal i "trace")
                (equal i "global")
                (equal i "constant")
                (equal i "class")
                (equal i "var")
		(equal i "defer")
                (equal i "asm")
                (equal i "mixin"))
            ; If it's a directive, cons a new directive to the recurse-list,
            ; and recurse.
            (ident-directive-helper (cdr s) (cons (make-dirc :contents i) l))
	    ; Otherwise, don't bother with making a new directive, just recurse.
            (ident-directive-helper (cdr s) (cons i l))))))


; THIS IS DA BIGGIE!  It reads a file, lexes it, and returns a list of tokens!
; Yaaaay!
(defun dancer-lex (filename)
   ; This setf makes things safer for interactive usage.
   (setf *file-contents* ())
   (slurp-file filename)
   (get-tokens)
   (ident-directives))

; This reads a file, parses it, and dumps the token-list to a file
; Debugging, yaay!
(defun lexer-dump (filename)
   (with-open-file 
      (s "dancerlex.out" 
         :direction :output 
	 :if-exists :overwrite
	 :if-does-not-exist :create)
      (format s "~A~%" (dancer-lex filename))))
