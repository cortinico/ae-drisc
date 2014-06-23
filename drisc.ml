(** DRISC Asm interpreter
		This file contain function for executing DRISC code.
		
    Year 2011
    
    Updated on 27-06-11 by Nicola Corti
*)

open Printf;;

(** DRISC default register number *)
(* Change this value if you want to change the default number of register *)
let regs = 32;;


(** DRISC default memory size *)
(* Change this value if you want to change the default memory size *)
let memsize = 128;;


(** shortcut to maps ... *)
type assoc = Ass of string * int;;


(** checks whether a key is in a map
		@param k A key to be found into a map
		@return true if k is found, false if not *)
let rec hasKey k = function 
   [] -> false
|  Ass(kk,vv)::rm -> if(kk = k) then true else (hasKey k rm);;


(** looks up a key in a map
		@param k The key to be found
		@param labs The map containing the key
		@return If the key is found, return his value, else it fail with "key not found"
*)
let rec valueOfKey k labs = 
  match labs with 
    [] -> failwith "key not found"
  | Ass(kk,vv)::rl -> if(kk=k) then vv else (valueOfKey k rl);;


(** execution environment (the state of the processor + 
    the labels compiled. 
    an environment is:
    pc, reg, mem, labels
 *)
type penv = 
  Penv of int ref * int array * int array * assoc list;;


(** pretty print the environment *)
let dump penv = 
  match penv with 
     Penv(pc,r,m,a) ->
	printf "PC=%d \n" !pc; 
        printf "%d registers. #MV = %d \n" 
		(Array.length r) (Array.length m);
        pp_reg_set r;
        pp_mem m
;;


(** Calculate the result of a non-jump instruction without increasing IC
		@param asm the ASM instruction
		@param env the enviroment where execute asm *)
let calculate asm env =
	match env with
		Penv(pc, r, m, labs) ->
	(match asm with
    ADD(Reg(a),Reg(b),Reg(c)) -> r.(c) <- r.(a) + r.(b);
|   SUB(Reg(a),Reg(b),Reg(c)) -> r.(c) <- r.(a) - r.(b);
|   MUL(Reg(a),Reg(b),Reg(c)) -> r.(c) <- r.(a) * r.(b);
|   DIV(Reg(a),Reg(b),Reg(c)) -> r.(c) <- r.(a) / r.(b);
|   ADDI(Reg(a),Const(b),Reg(c)) -> r.(c) <- r.(a) + b;
|   SUBI(Reg(a),Const(b),Reg(c)) -> r.(c) <- r.(a) - b;
|   INC(Reg(a)) -> r.(a) <- r.(a)+1;
|   DEC(Reg(a)) -> r.(a) <- r.(a)-1;
|   LD(Reg(a),Reg(b),Reg(c)) -> 
	let ind = r.(a) + r.(b) in 
	  r.(c) <- m.(ind);
|   LDI(Reg(a),Const(b),Reg(c)) -> 
	let ind = r.(a) + b in 
	  r.(c) <- m.(ind);
|   ST(Reg(a),Reg(b),Reg(c)) -> 
  let ind = r.(a) + r.(b) in
    m.(ind) <- r.(c);
|   STI(Reg(a),Const(b),Reg(c)) -> 
  let ind = r.(a) + b in
    m.(ind) <- r.(c);
|		_ -> printf "UNIMPLEMENTED:"; pp_asm asm
    )
;;


(** Function for handling the Delayed Branch execution,
		it execute the next instruction and change the value of IC.
		{b If in the Delay Slot there is a Jump, the execution fail}
 
		@param istr The ASM instruction in delay slot
		@param env The enviroment
		@param label The Label to jump to *)
let branch_handler istr env label =
	if (not(is_jump istr)) then
  	(printf "===========> DELAYED_BRANCH: executing";
  	pp_asm istr;
  	calculate istr env;
  	dump env;
  	match env with
	    Penv(pc, r, m, labs) ->
	  (match label with
    		LabLab(lbl) ->  let l = valueOfKey lbl labs in pc := l
    	| LabOff(off) ->  pc := !pc + off
    	| _ -> failwith "Label error"))
  else
  	failwith "Unable to execute a Jump after a Delayed Branch"
;;

	
(** execute one instruction within an environment 
    @param pgm the instruction to be executed 
    @param env the initial environment. it is modified via side effects *)
let step pgm env = 
  let apgm = Array.of_list (prog_to_asm pgm) in
  match env with 
    Penv(pc, r, m, labs) ->
  (let i = apgm.(!pc) in 
   if (not(is_jump i)) then
   		(calculate i env; pc:= !pc+1)
   else
   		(match i with 
|   CALL(Reg(f), Reg(ret)) -> 
	r.(ret) <- !pc + 1;
 	pc := r.(f)
|   GOTOR(Reg(l)) ->  pc := r.(l)
|   GOTOL(ll) -> 
		(match ll with 
			LabLab(lbl) ->  let l = valueOfKey lbl labs in pc := l
    | LabOff(off) ->  pc := !pc + off;
    | DelayedBranch(labl) -> 
    	let next = apgm.(!pc + 1) in branch_handler next env labl
    )
|   IFLEQ(Reg(r1),Reg(r2),l) ->
       if(r.(r1) <= r.(r2)) 
       then 
       	(match l with
       	LabOff(l) -> pc := !pc + l 
    |		LabLab(l) -> let lbl = valueOfKey l labs in pc := lbl
    |		DelayedBranch(labl) ->
    		let next = apgm.(!pc + 1) in branch_handler next env labl)
       else pc := !pc + 1
|   IFLE(Reg(r1),Reg(r2),l) ->
       if(r.(r1) < r.(r2)) 
       then 
       	(match l with
       	LabOff(l) -> pc := !pc + l 
    |		LabLab(l) -> let lbl = valueOfKey l labs in pc := lbl
    |		DelayedBranch(labl) ->
				let next = apgm.(!pc + 1) in branch_handler next env labl)
       else pc := !pc + 1
|   IFGEQ(Reg(r1),Reg(r2),l) ->
       if(r.(r1) >= r.(r2)) 
       then 
       	(match l with
       	LabOff(l) -> pc := !pc + l 
    |		LabLab(l) -> let lbl = valueOfKey l labs in pc := lbl
    |		DelayedBranch(labl) ->
				let next = apgm.(!pc + 1) in branch_handler next env labl)
       else pc := !pc + 1
|   IFGE(Reg(r1),Reg(r2),l) ->
       if(r.(r1) > r.(r2)) 
       then 
       	(match l with
       	LabOff(l) -> pc := !pc + l 
    |		LabLab(l) -> let lbl = valueOfKey l labs in pc := lbl
    |		DelayedBranch(labl) ->
				let next = apgm.(!pc + 1) in branch_handler next env labl)
       else pc := !pc + 1
|   IFEQ(Reg(r1),Reg(r2),l) ->
       if(r.(r1) = r.(r2)) 
       then 
       	(match l with
       	LabOff(l) -> pc := !pc + l 
    |		LabLab(l) -> let lbl = valueOfKey l labs in pc := lbl
    |		DelayedBranch(labl) ->
				let next = apgm.(!pc + 1) in branch_handler next env labl)
       else pc := !pc + 1
|   IFNEQ(Reg(r1),Reg(r2),l) ->
       if(not(r.(r1) = r.(r2))) 
       then 
       	(match l with
       	LabOff(l) -> pc := !pc + l 
    |		LabLab(l) -> let lbl = valueOfKey l labs in pc := lbl
    |		DelayedBranch(labl) ->
				let next = apgm.(!pc + 1) in branch_handler next env labl)
       else pc := !pc + 1
|   END -> failwith "Program terminated"

		(* Questo caso non e' mai raggiunto *)
| _ -> printf "UNIMPLEMENTED:"; pc := !pc + 1; pp_asm i))
;;

(** compile labels. Takes a program with labels and returns 
    a map with the label addresses
    @param pgm the program
    @param addr the initial address of the program  *)
let rec labels pgm addr = 
  match pgm with 
    [] -> []
  | i::ri -> 
      (match i with 
         Instr(i) -> (labels ri (addr+1))
       | LabInstr(LabLab(l),i) -> Ass(l,addr)::(labels ri (addr+1))
       | LabInstr(LabOff(l),i) -> failwith "Label Error, It's impossible to set an offset as an instruction label!"
       | LabInstr(DelayedBranch(l), i) -> failwith "Label Error, It's impossibile to set Delayed Branch as Istruction Label!"
      )
;;

(** Create a new set of register
		@param n The size of register set
		@return A new register set of size n *)
let create_regs n = 
  Array.create n 0;; 

(** Execute some step of an Instruction List
		@param r The reg set size
		@param m The memory size
		@param ipc The initial value of IC
		@param prg The instruciont list (the program)
		@param steps Number of steps to be executed *)
let stepper r m ipc prg steps = 
  let pc = ref ipc in 
  let reg = create_regs r in 
  let mem = create_regs m in 
  let penv = Penv(pc,reg,mem,(labels prg 0)) in 
  let aprg = Array.of_list prg in

  for i=0 to steps do
    printf "===========> STEP %d: executing " i; 
	(pp_instr !pc aprg.(!pc)); printf "\n";
    step prg penv; 
    dump penv;
  done
;;

(** Execute an Instruction List till the END instruction
		@param r The reg set size
		@param m The memory size
		@param ipc The initial value of IC
		@param prg The instruciont list (the program) *)
let execute r m ipc prg = 
	let pc = ref ipc in 
	let reg = create_regs r in 
	let mem = create_regs m in 
	let penv = Penv(pc, reg, mem,(labels prg 0)) in
	let aprg = Array.of_list prg in
	let i = (ref 0) in
	
	while (not(delab(aprg.(!pc)) = END)) do
		(
		printf "===========> STEP %d: executing " !i; (pp_instr !pc aprg.(!pc)); printf "\n";
		step prg penv;
		dump penv;
		i := !i + 1;
		)
	done
;;


(** Execute some step of an Instruction List
		with DRISC default value for reg set and memory
		@param prg The instruciont list (the program)
		@param steps Number of steps to be executed *)
let drisc_stepper prg step = 
	printf "Executing as a D-RISC Cpu \n";
	printf "Number of register: %d\n" regs;
	printf "Memory size: %d\n" memsize;
	printf "Number of steps: %d\n" step;
	stepper regs memsize 0 prg step
;;


(** Execute an Instruction List till the END instruction
		with DRISC default value for reg set and memory
		@param prg The instruciont list (the program) *)
let drisc_execute prg =
	printf "Executing as a D-RISC Cpu \n";
	printf "Number of register: %d\n" regs;
	printf "Memory size: %d\n" memsize;
	execute regs memsize 0 prg
;;


(** Calculate the max value between three value
		@param a The first value
		@param b The second value
		@param c The third value
		@return The max of previous value *)
let maxthree a b c =
	if (a > b) then
		(if (a > c) then a else c)
	else 
		(if (b > c) then b else c)
;;


(** Calculate the max register of a ASM instruction
		@param istr The ASM istruction
		@return The max register of an instruction *)
let maxregistr istr =
	match delab istr with
	 ADD(Reg(a),Reg(b),Reg(c)) -> maxthree a b c
|  SUB(Reg(a),Reg(b),Reg(c)) -> maxthree a b c
|  MUL(Reg(a),Reg(b),Reg(c)) -> maxthree a b c
|  DIV(Reg(a),Reg(b),Reg(c)) -> maxthree a b c
|  ADDI(Reg(a),Const(b),Reg(c)) -> if a < c then c else a
|  SUBI(Reg(a),Const(b),Reg(c)) -> if a < c then c else a
|  INC(Reg(a)) -> a
|  DEC(Reg(a)) -> a
|  LD(Reg(a),Reg(b),Reg(c)) -> maxthree a b c
|  ST(Reg(a),Reg(b),Reg(c)) -> maxthree a b c
|  LDI(Reg(a),Const(b),Reg(c)) -> if a < c then c else a
|  STI(Reg(a),Const(b),Reg(c)) -> if a < c then c else a
|  CALL(Reg(a),Reg(b)) -> if a < b then b else a
|  GOTOR(Reg(a)) -> a
|  IFLEQ(Reg(a), Reg(b), l) -> if a < b then b else a
|  IFLE(Reg(a), Reg(b), l) -> if a < b then b else a
|  IFGEQ(Reg(a), Reg(b), l) -> if a < b then b else a
|  IFGE(Reg(a), Reg(b), l) -> if a < b then b else a
|  IFEQ(Reg(a), Reg(b), l) -> if a < b then b else a
|  IFNEQ(Reg(a), Reg(b), l) -> if a < b then b else a
|  _ -> 0
;;


(** Calculate the max register of an instruction list
		@param prg The instruction list
		@param a The initial value (it must be set to 0, to let recursion work well)
		@return The max register of an instruction list *)
let rec maxreg prg a =
	match prg with
	[] -> a
	| x::xs -> let found = maxregistr x in 
		if found < a then maxreg xs a else maxreg xs found
;; 


(** Execute some step of an Instruction List
		with a Register set big enough for executing the instruction list
		@param m The memory size
		@param ipc The initial value of IC
		@param prg The instruciont list (the program)
		@param steps Number of steps to be executed *)
let autostepper m ipc prg steps = stepper (maxreg prg 0) m ipc prg steps
;;


(** Execute an Instruction List till the END instruction
		with a Register set big enough for executing the instruction list
		@param m The memory size
		@param ipc The initial value of IC
		@param prg The instruciont list (the program) *)
let autoexecute m ipc prg = execute (maxreg prg 0) m ipc prg
;;
