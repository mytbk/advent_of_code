with Ada.Containers.Vectors;
with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Integer_Text_Io;
use Ada.Integer_Text_Io;
with String_Util;
use String_Util;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure Advent_04 is
   type Board_Row is array (1 .. 5) of Integer;
   type Board_type is array (1 .. 5) of Board_Row;
   
   package Int_Vectors is new Ada.Containers.Vectors(Element_Type => Integer, Index_Type => Natural);
   subtype Int_Vec is Int_Vectors.Vector;
   
   function Get_Score(Board: Board_Type; Numbers: Int_Vec; Steps: out Integer) return Integer is
      type Marked_Row is array (1 .. 5) of Boolean;
      type Marked_board is array (1 .. 5) of Marked_Row;
      
      Marked_Cells: Marked_Board := (others => (others => False));
      Marked: Boolean;
      Last_Marked: Integer;
      Win : Boolean := False;
   begin
      Steps := 0;

      for Num of Numbers loop
	 Steps := Steps + 1;
	 Marked := False;
	 for Row in 1 .. 5 loop
	    exit when Marked;
	    for Column in 1 .. 5 loop
	       exit when Marked;
	       if Board(Row)(Column) = Num then
		  Marked_Cells(Row)(Column) := True;
		  Marked := True;
		  Last_Marked := Num;
		  exit;
	       end if;
	    end loop;
	 end loop;
	 for Row of marked_cells loop
	    if Row(1) and then Row(2) and then Row(3) and then Row(4) and then Row(5) then
	       Win := True;
	       exit;
	    end if;
	 end loop;
	 
	 for I in 1 .. 5 loop
	    if marked_cells(1)(I) and then marked_cells(2)(I) and then marked_cells(3)(I) and then marked_cells(4)(I) and then marked_cells(5)(I) then
	       Win := True;
	       exit;
	    end if;
	 end loop;
	 
	 if Win then
	    declare
	       Sum_Unmarked : Integer := 0;
	    begin
	       
	       for Row in 1 .. 5 loop
		  for Column in 1 .. 5 loop
		     if not Marked_Cells(Row)(Column) then
			Sum_Unmarked := Sum_Unmarked + Board(Row)(Column);
		     end if;
		  end loop;
	       end loop;
	       return Sum_Unmarked * Last_Marked;
	    end;
	    
	 end if;
      end loop;
      return 0; -- should not happen   
   end Get_Score;
   
   Min_Steps : Integer := 25;
   Max_Steps : Integer := 0;
   First_Win_Score : Integer;
   Last_Win_Score : Integer;
   Marked_Numbers: Int_Vec;
   Board: Board_Type;
      
begin
   -- read marked numbers
   declare
      L : String := Get_Line;
      Marked_strings : StringVec := Split_String(L, ',');
      Val: Integer;
      Unused_Last: Positive;
   begin
      for S of Marked_Strings loop
	 Get(To_String(S), Val, Unused_Last);
	 Marked_Numbers.Append(Val);
      end loop;
   end;
   
   -- read board
   while not End_Of_File loop
      for I in 1 .. 5 loop
	 for J in 1 .. 5 loop
	    Get(Board(I)(J));
	 end loop;
      end loop;
      
      declare
	 Step: Integer;
	 Board_Score: Integer;
      begin
	 Board_Score := Get_Score(Board, Marked_Numbers, Step);
	 if Step < Min_Steps then
	    Min_Steps := Step;
	    First_Win_Score := Board_Score;
	 end if;
	 
	 if Step > Max_Steps then
	    Max_Steps := Step;
	    Last_Win_Score := Board_Score;
	 end if;
      end;
   end loop;
   
   Put(First_Win_Score);
   Put(Last_Win_Score);
end Advent_04;
