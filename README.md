DRISC Ocaml Interpreter
========

This project is a little interpreter for DRISC code that analyze data deps inside the code. This project is part of the System Architecture Course at the Bachelor Degree in Computer Science in University of Pisa ([Course Page](http://didawiki.cli.di.unipi.it/doku.php/informatica/ae/start)).

The interpreter is written in OCaml, it's available the documentation written in [OCamlDoc](http://cortinico.github.io/ae-drisc/). Further information about this project, and newer version, can be found on [Didawiki Page](http://didawiki.cli.di.unipi.it/doku.php/informatica/ae/ocamldiplog).

### Executing the intepreted

For executing the interpreter you need the OCaml interpreter

* If you are under Ubuntu you can easily install with ```sudo apt-get install ocaml```
* Otherwise go to [OCaml Download Page](http://caml.inria.fr/download.en.html)

Then follow these steps:

```bash
git clone git@github.com:cortinico/ae-drisc.git
cd ae-drisc/
ocaml
```

Inside the ocaml environment, firts of all load the following files, that containt all the structures needed for the interpreter
```
#use "asm.ml"
#use "drisc.ml"
```

Then use the file **prog.ml** to write your ASM code using the following syntax
```ocaml
let p = [ 
  LabInstr(LabLab("loop"),LD(Reg(5),Reg(1),Reg(10)));
  Instr(LD(Reg(6),Reg(1),Reg(11)));
  Instr(ADD(Reg(10),Reg(11),Reg(12)));
  Instr(ST(Reg(7),Reg(1),Reg(12)));
  Instr(INC(Reg(1)));
  Instr(ADDI(Reg(1), Const(12), Reg(10)));
  Instr(ST(Reg(1), Reg(2), Reg(10)));
  Instr(IFLE(Reg(1),Reg(2),LabLab("loop")));
  Instr(END);
];;
```

And then load even the program file with
```
#use "prog.ml"
```

Now you can invoke the function ```pp_deps (data_deps p);;``` to get a pretty print of the data deps of p.
