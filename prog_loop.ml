let p = [
  LabInstr(LabLab("loop"),LD(Reg(5),Reg(8),Reg(10)));
  Instr(LD(Reg(6),Reg(8),Reg(11)));
  Instr(ADD(Reg(10),Reg(11),Reg(12)));
  Instr(ADDI(Reg(1), Const(9), Reg(1)));
  Instr(ADDI(Reg(1), Const(5), Reg(1)));
  Instr(INC(Reg(1)));
  Instr(IFLE(Reg(2),Reg(1),LabLab("loop")));
  Instr(END);
];;
