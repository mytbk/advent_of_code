with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Alu; use Alu;
with Ada.Containers.Vectors;

procedure Advent_24 is
   package Inst_Vectors is new Ada.Containers.Vectors
     (Element_Type => Instruction, Index_Type => Positive);
   subtype Inst_Vector is Inst_Vectors.Vector;
   
   Insts: Inst_Vector;
   
   type Model_Number is range 11_1111_1111 .. 10 ** 14 - 1;
   
   Mach: Machine_State;
   Input: Input_Stream(14);
   
   function Bfs_Search(Stream: in out Input_Stream; Insts: Inst_Vector; M: Machine_State; Cur_Pc: positive)
		      return Boolean is
      Pc: Positive := Cur_Pc;
      Snapshot: Machine_State := M;
      New_State: Machine_State;
      Cur : Positive := Stream.Cursor;
   begin
      while Pc <= Insts.Last_Index and then Insts(Pc).Op /= Inp loop
	 Execute_Instruction(Snapshot, Insts(Pc), Stream);
	 Pc := Pc + 1;
      end loop;
      
      if Pc > Insts.Last_Index then
	 if Snapshot.Registers(Z) = 0 then
	    return True;
	 else
	    return False;
	 end if;
      end if;
      
      -- now we find an input instruction, search from 9 to 1
      for I in reverse 1 .. 9 loop
	 Stream.Cursor := Cur;
	 Stream.Numbers(Cur) := I;
	 New_State := Snapshot;
	 Execute_Instruction(New_State, Insts(Pc), Stream);
	 if Bfs_Search(Stream, Insts, New_State, Pc + 1) then
	    return True;
	 end if;
      end loop;
      return False;
   end Bfs_Search;
begin
   while not End_Of_File loop
      declare
	 L: constant String := Get_Line;
      begin
	 Insts.Append(Assemble(L));
      end;
   end loop;
   
   Init_Machine(Mach);
   if Bfs_Search(Input, Insts, Mach, Insts.First_Index) then
      for I in 1 .. 14 loop
	 Put(Input.Numbers(I), Width => 1);
      end loop;
      New_Line;
   else
      Put_Line("Not found");
   end if;
end Advent_24;
