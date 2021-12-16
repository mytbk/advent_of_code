package body Packets is
   function Parse_Packet(Packet_Bin: Binary_String; Packet_Last: out Positive) return Packet_Access is
      Version: constant Natural := Binary_To_Number
	(Packet_Bin(Packet_Bin'First .. Packet_Bin'First + 2));
      Type_Id: constant Natural := Binary_To_Number
	(Packet_Bin(Packet_Bin'First + 3 .. Packet_Bin'First + 5));
      
      Pkt: Packet_Access := new Packet'(Type_Id => Type_Id, Version => Version, others => <>);
   begin
      if Type_Id = 4 then
	 -- literal value
	 declare
	    Lit_Val: Packet_Value := 0;	   
	    Last: Positive := Packet_Bin'First + 5;
	    Prefix_Index : Positive;
	    Last_Group : Boolean;
	 begin
	    loop
	       Prefix_Index := Last + 1;
	       Last_Group := Packet_Bin(Prefix_Index) = 0;
	       Last := Last + 5;
	       Lit_Val := Lit_Val * 16 + Packet_Value(Binary_To_Number
		 (Packet_Bin(Prefix_Index + 1 .. Prefix_Index + 4)));
	       exit when Last_Group;
	    end loop;
	    Packet_Last := Last;
	    Pkt.Literal_Value := Lit_Val;
	    return Pkt;
	 end;
      else
	 -- operator
	 if Packet_Bin(Packet_Bin'First + 6) = 0 then
	    declare
	       Sub_Packet_Bits : constant Positive := Binary_To_Number
		 (Packet_Bin(Packet_Bin'First + 7 .. Packet_Bin'First + 21));
	       Last_Bit_Index : constant Positive := Packet_Bin'First + 21 + Sub_Packet_Bits;
	       Last: Positive := Packet_Bin'First + 21;
	       Last_Out: Positive;
	    begin
	       while Last /= Last_Bit_Index loop
		  Pkt.Sub_Packets.Append(Parse_Packet(Packet_Bin(Last + 1 .. Last_Bit_Index), Last_Out));
		  Last := Last_Out;
	       end loop;
	       Packet_Last := Last;
	       return Pkt;
	    end;
	 else
	    declare
	       Sub_Packet_Num : constant Positive := Binary_To_Number
		 (Packet_Bin(Packet_Bin'First + 7 .. Packet_Bin'First + 17));
	       Last: Positive := Packet_Bin'First + 17;
	       Last_Out: Positive;
	    begin
	       for I in 1 .. Sub_Packet_Num loop
		  Pkt.Sub_Packets.Append(Parse_Packet(Packet_Bin(Last + 1 .. Packet_Bin'Last), Last_Out));
		  Last := Last_Out;
	       end loop;
	       Packet_Last := Last;
	       return Pkt;
	    end;
	 end if;
      end if;
   end Parse_Packet;
   
   function Sum_Packet_Version(Pkt: Packet) return Natural is
      Sum: Natural := Pkt.Version;
   begin
      if Pkt.Type_Id /= 4 then
	 for Sub of Pkt.Sub_Packets loop
	    Sum := Sum + Sum_Packet_Version(Sub.all);
	 end loop;
      end if;
      return Sum;
   end Sum_Packet_Version;
   
   function Evaluate_Packet(Pkt: Packet) return Packet_Value is
      Val: Packet_Value;
   begin
      case Pkt.Type_Id is
	 when 4 =>
	    return Pkt.Literal_Value;
	 when 0 =>
	    Val := 0;
	    for Sub of Pkt.Sub_Packets loop
	       Val := Val + Evaluate_Packet(Sub.all);
	    end loop;
	    return Val;
	 when 1 => 
	    Val := 1;
	    for Sub of Pkt.Sub_Packets loop
	       Val := Val * Evaluate_Packet(Sub.all);
	    end loop;
	    return Val;
	 when 2 =>
	    Val := Packet_Value'Last;
	    for Sub of Pkt.Sub_Packets loop
	       Val := Packet_Value'Min(Val, Evaluate_Packet(Sub.all));
	    end loop;
	    return Val;
	 when 3 =>
	    Val := Packet_Value'First;
	    for Sub of Pkt.Sub_Packets loop
	       Val := Packet_Value'Max(Val, Evaluate_Packet(Sub.all));
	    end loop;
	    return Val;
	 when 5 =>
	    if Evaluate_Packet(Pkt.Sub_Packets(1).all) > Evaluate_Packet(Pkt.Sub_Packets(2).all) then
	       return 1;
	    else
	       return 0;
	    end if;
	 when 6 =>
	    if Evaluate_Packet(Pkt.Sub_Packets(1).all) < Evaluate_Packet(Pkt.Sub_Packets(2).all) then
	       return 1;
	    else
	       return 0;
	    end if;
	 when 7 =>
	    if Evaluate_Packet(Pkt.Sub_Packets(1).all) = Evaluate_Packet(Pkt.Sub_Packets(2).all) then
	       return 1;
	    else
	       return 0;
	    end if;
	 when others =>
	    raise Constraint_Error;	  
      end case;
   end Evaluate_Packet;
   
   function Hex_Digit_To_Bin_String(C: Character) return Binary_String is
      subtype Hex_Range is Natural range 0 .. 15;
      Hex_Val : Hex_Range;
      Res: Binary_String(1 .. 4);
   begin
      if C >= '0' and then C <= '9' then
	 Hex_Val := Character'Pos(C) - Character'Pos('0');
      elsif C >= 'A' and then C <= 'F' then
	 Hex_Val := Character'Pos(C) - Character'Pos('A') + 10;
      else
	 raise Constraint_Error;
      end if;
      for I in reverse 1 .. 4 loop
	 Res(I) := Hex_Val mod 2;
	 Hex_Val := Hex_Val / 2;
      end loop;
      return Res;
   end Hex_Digit_To_Bin_String;

   function Hex_To_Bin_String(S: String) return Binary_String is
      Res: Binary_String(1 .. S'Length * 4);
      Res_Idx: Positive := 1;
      S_Idx: Positive := S'First;
   begin
      while S_Idx <= S'Last loop
	 Res(Res_Idx .. Res_Idx + 3) := Hex_Digit_To_Bin_String(S(S_Idx));
	 S_Idx := S_Idx + 1;
	 Res_Idx := Res_Idx + 4;
      end loop;
      return Res;
   end Hex_To_Bin_String;
   
   function Binary_To_Number(S: Binary_String) return Natural is
      Res: Natural := 0;
   begin
      for B of S loop
	 Res := Res * 2 + Natural(B);
      end loop;
      return Res;
   end Binary_To_Number;

end Packets;
