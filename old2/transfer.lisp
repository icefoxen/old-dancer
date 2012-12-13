; LEXER
; 
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

; PARSER
; Anyhoo, here's the wossnames.
; identifier      = identifier :: identifier 
;                   | anything that isn't matched as something else.
; value           = num | chr |  strliteral | symbolliteral | block
; expression      = [(] identifier expression expression* [)] 
;                   | value | identifier
; block           = [ paramdecl statementlist ]
; var             = VAR [( [identifier] [, identifier] )] [= expression]
; varlist         = var [, var]*
; asm             = ASM strliteral
; statement       = expression | var | asm
; statementseq    = statement [, statement]*
;
; paramitem       = [symbolliteral | symbolliteral = value
;                   | symbolliteral = [||] ]
; paramdecl       = ( [paramitem [, paramitem]*] )
;
; inheritancelist = (identifier identifier*)
;
; namelist        = [| identifier+ |]
; directive       = importbody | usebody | exportbody | tracebody 
;                   | globalbody |constantbody | mixinbody | classbody 
;                   | methodbody | method+body
; importbody      = IMPORT identifier [namelist] PERIOD
; usebody         = USE identifier [namelist] PERIOD
; exportbody      = EXPORT identifier [namelist] PERIOD
; tracebody       = TRACE identifier PERIOD
; globalbody      = GLOBAL identifier PERIOD
; constantbody    = CONSTANT identifier = value PERIOD
; mixinbody       = MIXIN identifier PERIOD
; classbody       = CLASS identifier inheritancelist [strliteral] 
;                   [varlist] PERIOD
; methodbody      = METHOD identifier identifier paramdecl [strliteral] 
;                   [statementseq] PERIOD
; method+body     = METHOD+ identifier identifier paramdecl [strliteral] 
;                   [statementseq] PERIOD
;
; list            = [| expression* |]



; This contains the name of the current directive, used for printing debugging
; info.
; Grr....  This CAN be improved, but it requires re-writing the lexer to
; include a :NEWLINE symbol or some such...  Well, it can be done I suppose.
; Not too hard either.  It's just messy.
(defglobal *current-directive* ())

; This is what everything is output to.
(defglobal *statement-list* ())

; Other important globals are *symbol-list*, which is where the input comes
; from.  It's imported from dancer-lexer.lisp


#|
Maybe have sequences define numbered methods?  A la Arc:
(a 5) == (aref a 5)
5 a   == ref a 5

Well...  Numbers are values, not symbols.  You can't really assign methods to
them/use them as messages...  Can ye?  No reason why not, sorta, but...  It
makes the compiler more complex.  Besides, we already have first/second/third
(though it's hard to have those as a return value or variable...  Well,
messages are automatically not-evaluated already, aren't they...?

Hrm... well, there actually seems to be nothing against a number as an
identifier in the syntax rules, so...  But then symbols must be quoted as
literals, which makes things MUCH ickier!
'foo bar bop
instead of 
foo bar bop
But... do you have to do this anyway if you want to be able to use expressions
to determine a message?  So in other words can you do 
(if (= x y) ['foo] ['bar]) Object args
???  It would be damn neat, but it would also necessitate quoting EVERY
message call, which is incredibly oogly.

Additional syntax for sequences here?  Or anything, really.  "foo=>bar"
compiles to "ref foo bar"?  But it interferes with the freedom of certain
methods; >= and <= and <=> and all those!  Maybe foo:bar?  Easy to confuse
with package syntax there, but it IS valid...  or :foo bar or some such...
Hrm.
|#

