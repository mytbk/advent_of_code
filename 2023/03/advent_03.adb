with Ada.Text_Io; use Ada.Text_Io;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Vectors;

procedure Advent_03 is
   type String_Acc is access String;
   
   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => String_Acc);
   Schematic : String_Vectors.Vector;
   
   -- part 1
   Sum_Part_Number : Integer := 0;
   
   -- part 2
   type Part_Number_Array is array (1 .. 2) of Integer;
   type Star_Info is record
      -- position
      X, Y : Positive;
      Num_Adjacent : Positive;
      Part_Numbers : Part_Number_Array;
   end record;
   
   package Star_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Star_Info);   
   All_Stars : Star_Vectors.Vector;
   Sum_Gears : Integer := 0;
begin
   while not End_Of_File loop
      Schematic.Append(new String'(Get_Line));
   end loop;
   
   for I in Schematic.First_Index .. Schematic.Last_Index loop
      declare
	 L : String renames Schematic(I).all;
	 First_Col, Last_Col : Positive;
	 Has_Adjacent_Symbol : Boolean;
	 Part_Number : Integer;
      begin
	 First_Col := L'First;
	 
	 while First_Col <= L'Last loop
	    while First_Col <= L'Last and then
	      not Is_Decimal_Digit(L(First_Col)) loop
	       First_Col := First_Col + 1;
	    end loop;
	    
	    if First_Col <= L'Last then
	       Last_Col := First_Col;
	       while Last_Col <= L'Last and then
		 Is_Decimal_Digit(L(Last_Col)) loop
		  Last_Col := Last_Col + 1;
	       end loop;
	       
	       Last_Col := Last_Col - 1;
	       Part_Number := Integer'Value(L(First_Col .. Last_Col));
	       -- now we find the last column of the number
	       -- we find the adjacent symbol now
	       Has_Adjacent_Symbol := False;
	       for X in I - 1 .. I + 1 loop
		  if X >= Schematic.First_Index and then
		    X <= Schematic.Last_Index then
		     declare
			Checked_Line : String renames Schematic(X).all;
		     begin
			for Y in First_Col - 1 .. Last_Col + 1 loop
			   if Y >= Checked_Line'First and Y <= Checked_Line'Last then
			      if not (Is_Decimal_Digit(Checked_Line(Y)) or
					Checked_Line(Y) = '.') then
				 Has_Adjacent_Symbol := True;
				 
				 if Checked_Line(Y) = '*' then
				    declare
				       Is_Saved : Boolean := False;
				    begin
				       for Star of All_Stars loop
					  if Star.X = X and Star.Y = Y then
					     Is_Saved := True;
					     Star.Num_Adjacent := Star.Num_Adjacent + 1;
					     if Star.Num_Adjacent <= 2 then
						Star.Part_Numbers(Star.Num_Adjacent) := Part_Number;
					     end if;
					  end if;
				       end loop;
				       if not Is_Saved then
					  All_Stars.Append
					    (Star_Info'
					       (X => X, Y => Y,
						Num_Adjacent => 1,
						Part_Numbers => (Part_Number, 0)));
				       end if;
				    end;
				 end if;   
			      end if;
			   end if;
			end loop;
		     end;
		  end if;
	       end loop;
	       
	       if Has_Adjacent_Symbol then
		  Sum_Part_Number := Sum_Part_Number + Part_Number;
	       end if;
	       
	       First_Col := Last_Col + 1;
	    end if;
	 end loop;
      end;
   end loop;
   
   -- part 1
   Put_Line(Integer'Image(Sum_Part_Number));
   
   -- part 2
   for Star of All_Stars loop
      if Star.Num_Adjacent = 2 then
	 Sum_Gears := Sum_Gears + Star.Part_Numbers(1) * Star.Part_Numbers(2);
      end if;
   end loop;
   Put_Line(Integer'Image(Sum_Gears));
end Advent_03;
