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
