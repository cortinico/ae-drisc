(** some support to the analysis of D-RISC code on pipeline processors
    This includes the possibility to find dependencies in linear 
    D-RISC code and to execute D-RISC instructions with a given 
    configuration of registers and memory (with no cache).
    @author Marco Danelutto 
    Year 2011
    
    Updated on 27-06-11 by Nicola Corti
*)

open Printf;;

(** the type modelling registers *)
type reg = Reg of int;;

(** the type modelling labels: either strings or offsets *)
type label = LabOff of int | LabLab of string | DelayedBranch of label;;

(** the type modelling the constants: e.g. #i -> Const(i) *)
type const = Const of int;;

(** the assembler opcodes *)
type asm = 
   ADD of reg*reg*reg
|  SUB of reg*reg*reg
|  MUL of reg*reg*reg
|  DIV of reg*reg*reg
|  ADDI of reg*const*reg
|  SUBI of reg*const*reg
|  INC of reg
|  DEC of reg
|  LD of reg*reg*reg
|  LDI of reg*const*reg
|  ST of reg*reg*reg
|  STI of reg*const*reg
|  CALL of reg*reg
|  GOTOR of reg
|  GOTOL of label
|  IFLEQ of reg*reg*label
|  IFLE of reg*reg*label
|  IFGEQ of reg*reg*label
|  IFGE of reg*reg*label
|  IFEQ of reg*reg*label
|  IFNEQ of reg*reg*label
|  END
;;

(** the assembler instruction: may have a label *)
type instruction = 
   Instr of asm 
|  LabInstr of label*asm;;

(** returns the domain of an instruction *)
let domain = function 
   ADD(a,b,c) -> [a;b]
|  SUB(a,b,c) -> [a;b]
|  MUL(a,b,c) -> [a;b]
|  DIV(a,b,c) -> [a;b]
|  ADDI(a,b,c) -> [a]
|  SUBI(a,b,c) -> [a]
|  INC(a) -> [a]
|  DEC(a) -> [a]
|  LD(a,b,c) -> [a;b]
|  ST(a,b,c) -> [a;b;c]
|  LDI(a,b,c) -> [a]
|  STI(a,b,c) -> [a;c]
|  CALL(a,b) -> [a]
|  GOTOR(a) -> [a]
|  GOTOL(a) -> []
|  IFLEQ(a,b,c) -> [a;b]
|  IFLE(a,b,c) -> [a;b]
|  IFGEQ(a,b,c) -> [a;b]
|  IFGE(a,b,c) -> [a;b]
|  IFEQ(a,b,c) -> [a;b]
|  IFNEQ(a,b,c) -> [a;b]
|  END -> []
;;
	
(** returns the range of an instruction *)
let codomain = function 
   ADD(a,b,c) -> [c]
|  SUB(a,b,c) -> [c]
|  MUL(a,b,c) -> [c]
|  DIV(a,b,c) -> [c]
|  ADDI(a,b,c) -> [c]
|  SUBI(a,b,c) -> [c]
|  INC(a) -> [a]
|  DEC(a) -> [a]
|  LD(a,b,c) -> [c]
|  ST(a,b,c) -> []
|  LDI(a,b,c) -> [c]
|  STI(a,b,c) -> []
|  CALL(a,b) -> []
|  GOTOR(a) -> []
|  GOTOL(a) -> []
|  IFLEQ(a,b,c) -> []
|  IFLE(a,b,c) -> []
|  IFGEQ(a,b,c) -> []
|  IFGE(a,b,c) -> []
|  IFEQ(a,b,c) -> []
|  IFNEQ(a,b,c) -> []
|  END -> []
;;

(** function computing the intersection of two lists. 
    This is used to compute Bernstein conditions
    @param l1 First list
    @param l2 Second list
    @return An empty list if no intersection is found, otherwise return the intersection list *)
let intersect l1 l2 = 
  let a1 = Array.of_list l1 in
  let a2 = Array.of_list l2 in
  let res = ref [] in 
  let n1 = Array.length a1 in 
  let n2 = Array.length a2 in 
  for i=0 to (n1-1) do 
    for j=0 to (n2-1) do 
      if(a1.(i) = a2.(j))
      then res := a2.(j) :: !res 
    done
  done; 
  !res
;;

(** checks if an instruction is "executed" on the IU *)
let iu_instruction = function
    IFLEQ(a,b,c) -> true
  | IFLE(a,b,c)  -> true
  | IFGEQ(a,b,c) -> true
  | IFGE(a,b,c)  -> true
  | IFEQ(a,b,c)  -> true
  | IFNEQ(a,b,c) -> true
  | LD(a,b,c)    -> true
  | LDI(a,b,c)   -> true
  | ST(a,b,c)    -> true
  | STI(a,b,c)   -> true
  | _ -> false
;;


(** data dependency: 
	instructions inducing the data dependencies
	interested register(s) 
	"distance"
	"N"
*) 
type datadep = 
  NoDataDep 
| DataDep of int * asm * int * asm * reg list * int * int ;;

(** removes labels from an instruction, if present, 
    and returns the assembler instruction only *)
let delab = function
  LabInstr(l,i) -> i 
| Instr(i) -> i;;

(** check whether there is a data dependency among instructions
    @param a1 the address of the first instruction
    @param a2 the address of the second instruction
    @param li1 the first instruction 
    @param li2 the second instruction 
    @param dist the distance between instructions 
    @param n the N parameter
    @return A new datadep instance with the previous parameters
*)
let data_dep_i a1 a2 li1 li2 dist n = 
  let i1 = delab li1 in 
  let i2 = delab li2 in
  let wrs = codomain(i1) in
  let rds = domain(i2) in 
  let regset = (intersect rds wrs) in 
  if(iu_instruction i2 && not(regset = []))
    then DataDep(a1,i1,a2,i2,(intersect rds wrs),dist,n)
    else NoDataDep;;

(** checks whether there is a load in the sequence 
    leading to the dependency 
    @param i1 the starting point of the sequence
    @param i2 the ending point of the sequence 
    @param prog the program with the sequence
    @return true if a LD or a LDI is found in the sequence, otherwise return false*)
let loadsinsequence prog i1 i2 = 
  let aprog = Array.of_list prog in 
  let bign  = ref false in 
  for i=i1 to (i2-1) do
    let asmi = match aprog.(i) with 
	    		Instr(ai) -> ai
	       |LabInstr(l,ai) -> ai in
    bign := match asmi with 
					LD(a,b,c) -> true
	    |   LDI(a,b,c) -> true
	    |   _ -> !bign
  done; 
  !bign
;; 

(** finds all data dependencies in a program 
    @param prog the program
    @return An empty list if no data dependency is found, otherwise return a list of data dependencies *)
let data_deps prog = 
  let aprog = Array.of_list prog in
  let n     = Array.length aprog in 
  let res   = ref [] in
  let start = ref 0 in 
  for i=0 to (n-2) do
    for j=(i+1) to (n-1) do
      let i1 = aprog.(i) in 
      let i2 = aprog.(j) in 
      let dd = (data_dep_i i j i1 i2 (j-i) 0) in (* N=0 TODO *)
        match dd with 
          NoDataDep -> ()
        | DataDep(i,i1,j,i2,regs,dist,n) -> 
	    let hasloads = loadsinsequence prog !start (j-1) in
   	    let bign = if(hasloads) then 2 else 1 in
            let dd = DataDep(i,i1,j,i2,regs,dist,bign) in

	    start := j;
	    res := (List.append (!res) [dd])
    done
  done;
  !res
;;

(** bernstein conditions check *)
let bernstein i1 i2 = 
  let d1 = domain i1 in 
  let d2 = domain i2 in 
  let c1 = codomain i1 in 
  let c2 = codomain i2 in
  let d1c2 = intersect d1 c2 in 
  let d2c1 = intersect d2 c1 in 
  if(d1c2 = [] && d2c1 = []) 
  then true
  else false
;;

(** pretty print a register *) 
let pp_reg = function
  Reg(x) -> printf " R_%d " x ;;

(** pretty print a list of registers *)
let rec pp_regs = function 
  [] -> ()
| r::rr -> (pp_reg r);(pp_regs rr);;

(** pretty print the register set
    Used in pretty print of the 
    environment of execution of a program 
    @param r An integer array rappresenting the register set *)
let pp_reg_set r = 
  let n = Array.length r in 
  for i=0 to (n-1) do
    printf " R%d=%d " i r.(i);
    if (i mod 16 == 0 && not(i = 0)) then printf "\n";
  done; 
  printf "\n"
;;

(** pretty print a memory state. Used in pretty print of the 
    environment of execution of a program
    @param r An integer array rappresenting the memory *)
let pp_mem r = 
  let n = Array.length r in 
  for i=0 to (n-1) do
    if(i mod 8 == 0 && not (i = 0)) 
    then printf "\n";
    if(i mod 4 == 0) 
    then printf "M%d=%d\t" i r.(i)
    else printf "%d\t" r.(i)
  done; 
  printf "\n"
;;

(** pretty print a label *)
let pp_lab = function 
  LabLab(s) -> printf " %s " s
| LabOff(o) -> printf "L(%d) " o
| DelayedBranch(LabLab(s)) -> printf " %s , delayed_branch" s
| DelayedBranch(LabOff(o)) -> printf " L(%d) , delayed_branch" o
| _ -> printf "Label Error"
;;

(** pretty print a constant *)
let pp_const = function 
  Const c -> printf " #%d " c;;

(** pretty print a D-RISC instruction *)
let pp_asm = function 
  ADD(a,b,c) -> printf "ADD "; pp_reg(a); pp_reg(b); pp_reg(c)
| SUB(a,b,c) -> printf "SUB "; pp_reg(a); pp_reg(b); pp_reg(c)
| MUL(a,b,c) -> printf "MUL "; pp_reg(a); pp_reg(b); pp_reg(c)
| DIV(a,b,c) -> printf "DIV "; pp_reg(a); pp_reg(b); pp_reg(c)
| ADDI(a,b,c) -> printf "ADDI "; pp_reg(a); pp_const(b); pp_reg(c)
| SUBI(a,b,c) -> printf "SUBI "; pp_reg(a); pp_const(b); pp_reg(c)
| INC(a) -> printf "INC"; pp_reg(a)
| DEC(a) -> printf "DEC"; pp_reg(a)
| LD(a,b,c) -> printf "LOAD "; pp_reg(a); pp_reg(b); pp_reg(c)
| LDI(a,b,c) -> printf "LOAD_I "; pp_reg(a); pp_const(b); pp_reg(c)
| ST(a,b,c) -> printf "STORE "; pp_reg(a); pp_reg(b); pp_reg(c)
| STI(a,b,c) -> printf "STORE_I "; pp_reg(a); pp_const(b); pp_reg(c)
| CALL(a,b) -> printf "CALL "; pp_reg(a); pp_reg(b)
| GOTOR(a) -> printf "GOTO_R "; pp_reg(a)
| GOTOL(l) -> printf "GOTO_L "; pp_lab(l)
| IFLE(r1,r2,l) -> printf "IF< "; pp_reg(r1); pp_reg(r2); pp_lab(l)
| IFLEQ(r1,r2,l) -> printf "IF<= "; pp_reg(r1); pp_reg(r2); pp_lab(l)
| IFGE(r1,r2,l) -> printf "IF> "; pp_reg(r1); pp_reg(r2); pp_lab(l)
| IFGEQ(r1,r2,l) -> printf "IF>= "; pp_reg(r1); pp_reg(r2); pp_lab(l)
| IFEQ(r1,r2,l) -> printf "IF= "; pp_reg(r1); pp_reg(r2); pp_lab(l)
| IFNEQ(r1,r2,l) -> printf "IF<> "; pp_reg(r1); pp_reg(r2); pp_lab(l)
| END -> printf "END "
| _ -> printf "NOFORMATAVAILABLE"
;;

(** pretty print an instruction, possibly with a label *)
let pp_instr add = function 
  Instr(i) -> printf "%d.\t" add; pp_asm(i); printf "\n"
| LabInstr(l,i) -> printf "%d. " add; 	
		   pp_lab(l); printf ":"; pp_asm(i); printf "\n"
;;

(** pretty print a whole program, starting with at a given address *)
let rec pp_prog add = function 
  [] -> printf "\n"
| i::ri -> (pp_instr add i); (pp_prog (add+1) ri)
;;

(** pretty print a program, assuming it is allocated from address 0 *)
let pp_program p = (pp_prog 1 p);;


(** pretty print a data dependency *) 
let pp_dd = function 
  NoDataDep -> ()
| DataDep(a1,i1,a2,i2,regl,d,n) -> 
   printf "DD:: "; 
   printf "%d. " a1; pp_asm i1; printf " ==>> "; 
   printf "%d. " a2; pp_asm i2; 
   printf " (d=%d N=%d) due to reg(s)" d n;
   pp_regs regl;
   printf "\n";;

(** pretty print a list of dependencies *)
let rec pp_deps = function 
   [] -> ()
|  d::rd -> (pp_dd d);(pp_deps rd);;

(** transforms a list of instructions (with labels) into  
    a list of assembler instruction (with no labels) *)
let rec prog_to_asm p = 
  List.map delab p;;
  
(* 
=============================

Gestione dei Salti

=============================
*)

(** Check if an ASM instruction in a jump istruction
		@param istr the ASM instruction
		@return true if istr is a jump instruction, false in other case *)
let is_jump istr =
	match istr with
   CALL(a,b) -> true
|  GOTOR(a) -> true
|  GOTOL(a) -> true
|  IFLEQ(a, b, l) -> true
|  IFLE(a, b, l) -> true
|  IFGEQ(a, b, l) -> true
|  IFGE(a, b, l) -> true
|  IFEQ(a, b, l) -> true
|  IFNEQ(a, b, l) -> true
|  _ -> false
;;

(** Check if an ASM instruction has Delayed Branch flag
		@param istr the ASM instruction
		@return true if istr has DelayedBranch, false in other case *)
let has_delayed_branch istr =
	match istr with
   GOTOL(DelayedBranch(a)) -> true
|  IFLEQ(a, b, DelayedBranch(l)) -> true
|  IFLE(a, b, DelayedBranch(l)) -> true
|  IFGEQ(a, b, DelayedBranch(l)) -> true
|  IFGE(a, b, DelayedBranch(l)) -> true
|  IFEQ(a, b, DelayedBranch(l)) -> true
|  IFNEQ(a, b, DelayedBranch(l)) -> true
|  _ -> false
;;


(** Jumps:
		int: instruction number
		asm: the instruction
*)
type jumps = 
  NoJump 
| Jump of int * asm;;


(** finds all jumps in a program 
    @param prog the program
    @return An empty list if no jump is found, otherwise return a list of jumps *)
let jump_find prog =
	let asmprog = prog_to_asm prog in 
  let aprog = Array.of_list asmprog in
  let n     = Array.length aprog in 
  let res   = ref [] in
  for i=0 to n-1 do
    let istr = aprog.(i) in 
			if (is_jump istr && not(has_delayed_branch istr)) then
	    	res := (List.append (!res) [Jump(i, istr)])
  done;
  !res
;;

(** pretty print a jump *) 
let pp_jump = function 
  NoJump -> ()
| Jump(i, istr) -> 
   printf "JUMP:: "; 
   printf "%d. " i; pp_asm istr; 
   printf "\n"
;;

(** pretty print a list of jumps *)
let rec pp_jumps = function 
   [] -> ()
|  x::xs -> (pp_jump x);(pp_jumps xs);;

(* 
=============================
*)  
  
  
