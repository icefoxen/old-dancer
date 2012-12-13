(* dancer-rt.ml
   Basic Dancer runtime junkin... experimental

   Simon Heath
   16/9/2003
*)

exception RedefinedConstant of string;;
exception RedefinedClass of string;;
exception RedefinedMethod of string * string;;

(* A universal Dancer-value type.  All variables are of this type. *)
(* It's just a name and pointer to a real object in the symbol-table... *)
type d_var = {
   varname : string;
   mutable varptr : int
   };;

(* An arg-type.  The string is the name, the d_var is the default value.
   So "x" would be ("x", 0) (assuming 0 is NIL), "x=3" would be ("x",
   ptr-to-3), and so on.  There's probably a special value, -1 or something,
   for variable-length args.  *)
type d_arg = Arg of string * d_var;;

(* An arg-list type *)
type d_arglist = d_arg list;;


(* A method type. *)
type d_method = {
   name    : string;
   args    : d_arglist;
   lvars   : d_var array;
   code    : (unit -> unit) list;
   m_doc   : string;
   };;

(* A mixin type *)
type d_mixin = {
   name    : string;
   methods : d_method list;
   mx_doc  : string;
   };;


(* A class type *)
type d_class = {
   name       : string;
   superclass : d_class;
   mutable imethods   : d_method list;
   mutable cmethods   : d_method list;
   mixins     : d_mixin list;
   ivars      : d_var array;
   c_doc      : string;
   };;




(* A symbol-table entry type *)
(* 'Ang on... I can get rid of this, and just have the d_var's refer directly
   to the classes... 
   waitwait... a CLASS ISN'T an INSTANCE... *)
type d_symtable_entry = {
   index   : int;
   val_ptr : d_class;
   };;

(* A symbol-table type *)
type d_symtable = d_symtable_entry list ref;;


(* Primative number type... very dumb, no ratios, no complex, no bignums. *)
type d_number =
   DInt of int
 | DFloat of float;;

(* Number-table entry type *)
type d_numtable_entry = {
   indx   : int;
   val_pt : d_number;
   };;




(* Okay, the way scope works is thus:
   There is one d_symtable, which holds references to all existing classes
   except numbers.  There is the scope stack, which is full of arrays of
   d_vars, each of which is a name and pointer to some value on the symbol
   table.  
   When a method is entered, its lvar array is pushed onto the stack table,
   any necessary values are created, and the lvar array is set to point to
   them.  
   When a new class is entered, it's ivar array is pushed in the same manner,
   and popped when it's left.
   To reference a variable, it just searches down the stack checking names.

   There are also lists of global and constant variables, which are searched
   after the scope stack.
   *)

(* Symbol table which actually refers to objects *)
let symtable = ref [];;
let symcounter = ref 0;;

(* Stack to which var-arrays are pushed and popped *)
let scopestack = Stack.create ();;

(* bottom-level environment stuff *)
let globals = ref [];;
let constants = ref [];;

(* Welp, I guess this first version is gonna be an interpreter.  So we need to
   hang on to some data here...  *)
let classes = ref [];;
let mixins = ref [];;


(* The way arg-passing works is thus:
   When a method is called, all the args given are turned into a d_arglist.
   It is then compared to the method's d_arglist, reshuffled and re-ordered a
   bit as per names, and if they match in names and length, then it's all
   good, the values are shuffled out and poked onto the scope-stack, and the
   method runs.
*)


(* The way a instance method-call works is thus:
   The class it's called upon is stuck into scope.  It's methods are searched,
   and if the name matches the method called, then it runs.  Otherwise, the
   same thing happens to it's superclass, all the way up to Object.
   Once a method is found, the arg-lists are compared, and if they match up,
   the method is executed.

   TODO: Make them indexing, so a class remembers which parent handles the
   method called.  A la Obj-C, Smalltalk.
*)



(***************************************************************************)
(***   FUNCTIONS   *********************************************************)


(* These mess with classes and methods *)

let getClassByName nm = 
   List.find (fun x -> x.name = nm) !classes;;

let defClass n sc mi iv dc =
   if getClassByName n then
      raise RedefinedClass n
   else
      let i = {
         name = n;
         superclass = (getClassByName sc);
         imethods = ref [];
         cmethods = ref [];
         mixins = mi;
         ivars = iv;
         c_doc = dc;
      } in 
      classes := i :: !classes
   ;;


let getInstanceMethodByName m c =
   List.find (fun n -> (n.name = m)) c.imethods;;

let instanceMethodExists m c =
   List.exists (fun n -> n.name = m) c.imethods

let getClassMethodByName m c =
   List.find (fun n -> (n.name = m)) c.cmethods;;

let instanceMethodExists m c =
   List.exists (fun n -> (n.name = m)) c.cmethods;;




let defInstanceMethod nm al lv cd dc cls =
   if instanceMethodExists nm cls then
      raise RedefinedMethod (nm, cls)
   else
      let m = {
         name = nm;
         args = al;
         lvars = lv;
         code = cd;
         m_doc = dc;
      } 
      and c = getClassByName cls
      in
         c.imethods <- m :: c.imethods
   ;;
   
  
let defClassMethod nm al lv cd dc cls =
   if classMethodExists nm cls then
      raise RedefinedMethod (nm, cls)
   else
      let m = {
         name = nm;
         args = al;
         lvars = lv;
         code = cd;
         m_doc = dc;
      } 
      and c = getClassByName cls
      in
         c.cmethods <- m :: c.cmethods
   ;;


(* These add and find objects in the global symbol table, and mess with 
   globals and locals.  *)

let makeObject obj =
   let v = {
      index = !symcounter;
      val_ptr = obj
      }
   in
      symcounter := !symcounter + 1;
      symtable := v :: !symtable;;

let getObject ind =
   (List.find (fun x -> x.index = ind) !symtable).val_ptr;;

let getObjectIndex obj =
   (List.find (fun x -> x.val_ptr = ind) !symtable).index;;

let objectExists obj =
   (List.exists (fun x -> x.val_ptr = obj) !symtable);;



let makeGlobal vl =
   let _ = makeObject vl
   and x = getObjectIndex vl
   in
      if globalExists vl then
         let y = (List.filter (



let globalExists g =
   List.exists (fun n -> (n = m)) !globals;;

let constantExists c =
   List.exists (fun n -> (n = m)) !constants;;
