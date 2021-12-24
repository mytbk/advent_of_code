package Alu is
   type Variable is (W,X,Y,Z);
   type Register_File is array (Variable) of Integer;
   
   type Opcode is (Inp, Add, Mul, Div, Modulo, Eql);
   
   type Source_Operand is record
      Is_Variable: Boolean;
      Var: Variable;
      Num: Integer;
   end record;
   
   type Instruction is record
      Op: Opcode;
      Dst: Variable;
      Src: Source_Operand;
   end record;
   
   type Machine_State is record
      Registers: Register_File;
   end record;
   
   type Number_Array is array (Positive range <>) of Integer;
   type Input_Stream(Len: Positive) is record
      Cursor: Positive := 1;
      Numbers: Number_Array(1 .. Len);
   end record;
   
   function Get(Input: in out Input_Stream) return Integer;
   
   procedure Init_Machine(Machine: out Machine_State);
   procedure Execute_Instruction(Machine: in out Machine_State; Inst: Instruction; Input: in out Input_Stream);
   
   function Assemble(S: String) return Instruction;
end Alu;
