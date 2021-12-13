with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Text_Io; use Ada.Text_Io;

procedure Advent_13 is
   type Point is record
      X,Y: Integer;
   end record;
   
   function Point_Hash(P: Point) return Hash_Type is
   begin
      return Hash_Type(P.X) xor Hash_Type(P.Y);
   end Point_Hash;
   
   package Point_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Point, Hash => Point_Hash, Equivalent_Elements => "=");
   
   subtype Point_Set is Point_Sets.Set;
   
   function Parse_Point(S: String) return Point is
      P: Point;
      Unused_Last: Positive;
   begin
      for I in S'Range loop
	 if S(I) = ',' then
	    Get(S(S'First .. I - 1), P.X, Unused_Last);
	    Get(S(I + 1 .. S'Last), P.Y, Unused_Last);
	    return P;
	 end if;
      end loop;
      raise Constraint_Error;
   end Parse_Point;
   
   type Fold_Direction is (Up, Left);
   type Fold_Instruction is record
      Direction: Fold_Direction;
      Position: Integer;
   end record;
   
   package Fold_Instruction_Vectors is new Ada.Containers.Vectors
     (Element_Type => Fold_Instruction, Index_Type => Positive);
   
   subtype Fold_Instruction_Vector is Fold_Instruction_Vectors.Vector;

   function Parse_Fold_Instruction(S: String) return Fold_Instruction is
      Prefix: constant String := "fold along ";
      Fi: Fold_Instruction;
      Unused_Last: Positive;
   begin
      case S(S'First + Prefix'Length) is
	 when 'x' => Fi.Direction := Left;
	 when 'y' => Fi.Direction := Up;
	 when others => raise Constraint_Error;
      end case;
      Get(S(S'First + Prefix'Length + 2 .. S'Last), Fi.Position, Unused_Last);
      return Fi;
   end Parse_Fold_Instruction;
   
   function Fold(Points: Point_Set; Inst: Fold_Instruction) return Point_Set is
      New_Set: Point_Set;
      Unused_Position: Point_Sets.Cursor;
      Unused_Inserted: Boolean;
   begin
      for P of Points loop
	 case Inst.Direction is
	    when Up =>
	       if P.Y > Inst.Position then
		  New_Set.Insert((X => P.X, Y => Inst.Position * 2 - P.Y),
				 Unused_Position, Unused_Inserted);
	       else
		  New_Set.Insert(P, Unused_Position, Unused_Inserted);
	       end if;
	    when Left =>
	       if P.X > Inst.Position then
		  New_Set.Insert((X => Inst.Position * 2 - P.X, Y => P.Y),
				Unused_Position, Unused_Inserted);
	       else
		  New_Set.Insert(P, Unused_Position, Unused_Inserted);
	       end if;
	 end case;
      end loop;
      return New_Set;
   end Fold;
   
   Points: Point_Set;
   First_Fold_Set : Point_Set;
   Final_Fold_Set : Point_Set;
   Fold_Instructions: Fold_Instruction_Vector;
   
   subtype X_Range is Integer range 0 .. 40;
   subtype Y_Range is Integer range 0 .. 10;
   -- Final_Graph: array (X_Range, Y_Range) of Boolean := (others => False);
begin
   loop
      declare
	 L: constant String := Get_Line;
      begin
	 exit when L'Length = 0;
	 Points.Include(Parse_Point(L));
      end;
   end loop;
   
   while not End_Of_File loop
      declare
	 L: constant String := Get_Line;
      begin
	 Fold_Instructions.Append(Parse_Fold_Instruction(L));
      end;
   end loop;
   
   -- part 1
   First_Fold_Set := Fold(Points, Fold_Instructions.First_Element);
   Put_Line(Count_Type'Image(First_Fold_Set.Length));
   
   Final_Fold_Set := Points;
   for I of Fold_Instructions loop
      Final_Fold_Set := Fold(Final_Fold_Set, I);
   end loop;
   
   for Y in Y_Range'Range loop
      for X in X_Range'Range loop
	 if Final_Fold_Set.Contains((X => X, Y => Y)) then
	    Put("# ");
	 else
	    Put(". ");
	 end if;
      end loop;
      New_Line;
   end loop;
   
end Advent_13;
