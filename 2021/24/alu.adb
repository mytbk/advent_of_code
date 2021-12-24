with Ada.Integer_Text_Io;
use Ada.Integer_Text_Io;

package body Alu is
   function Get(Input: in out Input_Stream) return Integer is
      Num: constant Integer := Input.Numbers(Input.Cursor);
   begin
      Input.Cursor := Input.Cursor + 1;
      return Num;
   end Get;
   
   procedure Init_Machine(Machine: out Machine_State) is
   begin
      for V in Variable'Range loop
	 Machine.Registers(V) := 0;
      end loop;
   end Init_Machine;
   
   procedure Execute_Instruction(Machine: in out Machine_State; Inst: Instruction; Input: in out Input_Stream) is
      R: Register_File renames Machine.Registers;
   begin
      case Inst.Op is
	 when Inp =>
	    R(Inst.Dst) := Get(Input);
	 when Add =>
	    if Inst.Src.Is_Variable then
	       R(Inst.Dst) := R(Inst.Dst) + R(Inst.Src.Var);
	    else
	       R(Inst.Dst) := R(Inst.Dst) + Inst.Src.Num;
	    end if;
	 when Mul =>
	    if Inst.Src.Is_Variable then
	       R(Inst.Dst) := R(Inst.Dst) * R(Inst.Src.Var);
	    else
	       R(Inst.Dst) := R(Inst.Dst) * Inst.Src.Num;
	    end if;
	 when Div =>
	    if Inst.Src.Is_Variable then
	       R(Inst.Dst) := R(Inst.Dst) / R(Inst.Src.Var);
	    else
	       R(Inst.Dst) := R(Inst.Dst) / Inst.Src.Num;
	    end if;
	 when Modulo =>
	    if Inst.Src.Is_Variable then
	       R(Inst.Dst) := R(Inst.Dst) mod R(Inst.Src.Var);
	    else
	       R(Inst.Dst) := R(Inst.Dst) mod Inst.Src.Num;
	    end if;
	 when Eql =>
	    if Inst.Src.Is_Variable then
	       R(Inst.Dst) := (if R(Inst.Dst) = R(Inst.Src.Var) then 1 else 0);
	    else
	       R(Inst.Dst) := (if R(Inst.Dst) = Inst.Src.Num then 1 else 0);
	    end if;	    
      end case;
   end Execute_Instruction;
   
   function Assemble(S: String) return Instruction is
      Inst: Instruction;
      Unused_Last: Positive;
   begin
      if S(S'First .. S'First + 2) = "mod" then
	 Inst.Op := Modulo;
      else
	 Inst.Op := Opcode'Value(S(S'First .. S'First + 2));
      end if;
      
      Inst.Dst := Variable'Value(S(S'First + 4 .. S'First + 4));
      
      if Inst.Op = Inp then
	 return Inst;
      end if;
      
      -- when not inp, it has a second operand
      case S(S'First + 6) is
	 when 'x' | 'y' | 'z' | 'w' =>
	    Inst.Src.Is_Variable := True;
	    Inst.Src.Var := Variable'Value(S(S'First + 6 .. S'First + 6));
	 when others =>
	    Inst.Src.Is_Variable := False;
	    Get(S(S'First + 6 .. S'Last), Inst.Src.Num, Unused_Last); 
      end case;
      
      return Inst;
   end Assemble;
end Alu;
