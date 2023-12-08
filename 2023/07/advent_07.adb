with Ada.Text_Io; use Ada.Text_Io;
with Ada.Containers.Vectors;

procedure Advent_07 is
   type Card_Hand is new String(1 .. 5);
   type Hand_Kind is (Five_Of_A_Kind, Four_Of_A_Kind, Full_House,
		      Three_Of_A_Kind, Two_Pair, One_Pair, High_Card);
   type Card_Number_Map is array (Character) of Natural;
   
   Card_Ranks : constant array (Character) of Natural :=
     ('2' => 1, '3' => 2, '4' => 3, '5' => 4, '6' => 5, '7' => 6, '8' => 7,
      '9' => 8, 'T' => 9, 'J' => 10, 'Q' => 11, 'K' => 12, 'A' => 13,
      others => 0);
   
   -- make 'J' rank 0
   Alt_Card_Ranks : constant array (Character) of Natural :=
     ('2' => 1, '3' => 2, '4' => 3, '5' => 4, '6' => 5, '7' => 6, '8' => 7,
      '9' => 8, 'T' => 9, 'Q' => 11, 'K' => 12, 'A' => 13,
      others => 0);

   function Classify_Hand(H : Card_Hand) return Hand_Kind is
      Num_Cards : Card_Number_Map := (others => 0);
      Has_Three : Boolean := False;
      Num_Pairs : Natural := 0;
   begin
      for C of H loop
	 Num_Cards(C) := Num_Cards(C) + 1;
      end loop;
      
      for N of Num_Cards loop
	 if N = 5 then
	    return Five_Of_A_Kind;
	 end if;
	 
	 if N = 4 then
	    return Four_Of_A_Kind;
	 end if;
	 
	 if N = 3 then
	    Has_Three := True;
	 end if;
	 
	 if N = 2 then
	    Num_Pairs := Num_Pairs + 1;
	 end if;
      end loop;
      
      if Has_Three then
	 if Num_Pairs /= 0 then
	    return Full_House;
	 else
	    return Three_Of_A_Kind;
	 end if;
      end if;
      
      if Num_Pairs = 2 then
	 return Two_Pair;
      end if;
      
      if Num_Pairs = 1 then
	 return One_Pair;
      end if;
      
      return High_Card;
   end Classify_Hand;
   
   function Alt_Classify_Hand(H : Card_Hand) return Hand_Kind is
      Num_Cards : Card_Number_Map := (others => 0);
      Has_Three : Boolean := False;
      Num_Pairs : Natural := 0;
      Number_Of_J : Natural;
   begin
      for C of H loop
	 Num_Cards(C) := Num_Cards(C) + 1;
      end loop;
      
      Number_Of_J := Num_Cards('J');
      Num_Cards('J') := 0;
      
      for N of Num_Cards loop
	 if N = 5 then
	    return Five_Of_A_Kind;
	 end if;
	 
	 if N = 4 then
	    if Number_Of_J = 1 then
	       return Five_Of_A_Kind;
	    else
	       return Four_Of_A_Kind;
	    end if;
	 end if;
	 
	 if N = 3 then
	    if Number_Of_J = 2 then
	       return Five_Of_A_Kind;
	    elsif Number_Of_J = 1 then
	       return Four_Of_A_Kind;
	    end if;
	    
	    -- We assume we don't have 'J' when Has_Tree
	    Has_Three := True;
	 end if;
	 
	 if N = 2 then
	    if Number_Of_J = 3 then
	       return Five_Of_A_Kind;
	    elsif Number_Of_J = 2 then
	       return Four_Of_A_Kind;
	    end if;
	    
	    -- we assume number of 'J's is <=1 when has pairs
	    Num_Pairs := Num_Pairs + 1;
	 end if;
      end loop;
      
      if Has_Three then
	 if Num_Pairs /= 0 then
	    return Full_House;
	 else
	    return Three_Of_A_Kind;
	 end if;
      end if;
      
      if Num_Pairs = 2 then
	 if Number_Of_J /= 0 then
	    -- One 'J'
	    return Full_House;
	 else
	    return Two_Pair;
	 end if;
      end if;
      
      if Num_Pairs = 1 then
	 if Number_Of_J /= 0 then
	    return Three_Of_A_Kind;
	 else
	    return One_Pair;
	 end if;
      end if;
      
      -- all distinct except 'J's
      case Number_Of_J is
	 when 5 | 4 =>
	    return Five_Of_A_Kind;
	 when 3 =>
	    return Four_Of_A_Kind;
	 when 2 =>
	    return Three_Of_A_Kind;
	 when 1 =>
	    return One_Pair;
	 when 0 =>
	    return High_Card;
	 when others =>
	    raise Program_Error;
      end case;
   end Alt_Classify_Hand;

   function Hand_Weaker(H1, H2 : Card_Hand) return Boolean is
      Kind1, Kind2 : Hand_Kind;
   begin
      Kind1 := Classify_Hand(H1);
      Kind2 := Classify_Hand(H2);
      if Kind1 > Kind2 then
	 return True;
      elsif Kind1 < Kind2 then
	 return False;
      else
	 for I in Card_Hand'Range loop
	    if H1(I) /= H2(I) then
	       return Card_Ranks(H1(I)) < Card_Ranks(H2(I));
	    end if;
	 end loop;
      end if;
      
      -- identical hands
      return False;
   end Hand_Weaker;
   
   function Alt_Hand_Weaker(H1, H2 : Card_Hand) return Boolean is
      Kind1, Kind2 : Hand_Kind;
   begin
      Kind1 := Alt_Classify_Hand(H1);
      Kind2 := Alt_Classify_Hand(H2);
      if Kind1 > Kind2 then
	 return True;
      elsif Kind1 < Kind2 then
	 return False;
      else
	 for I in Card_Hand'Range loop
	    if H1(I) /= H2(I) then
	       return Alt_Card_Ranks(H1(I)) < Alt_Card_Ranks(H2(I));
	    end if;
	 end loop;
      end if;
      
      -- identical hands
      return False;
   end Alt_Hand_Weaker;

   type Hand_And_Bid is record
      Hand : Card_Hand;
      Bid : Integer;
   end record;
   
   procedure Parse_Hand_And_Bid(S : String; Hab : out Hand_And_Bid) is
   begin
      Hab.Hand := Card_Hand(S(S'First .. S'First + 4));
      Hab.Bid := Integer'Value(S(S'First + 6 .. S'Last));
   end Parse_Hand_And_Bid;
   
   package Hand_And_Bid_Vectors is new Ada.Containers.Vectors
     (Element_Type => Hand_And_Bid, Index_Type => Positive);
   subtype Hand_And_Bid_Vector is Hand_And_Bid_Vectors.Vector;
   
   function Weaker (H1, H2 : Hand_And_Bid) return Boolean is
   begin
      return Hand_Weaker(H1.Hand, H2.Hand);
   end Weaker;
   package Hab_Sorting is new Hand_And_Bid_Vectors.Generic_Sorting
     ("<" => Weaker);
   
   function Alt_Weaker (H1, H2 : Hand_And_Bid) return Boolean is
   begin
      return Alt_Hand_Weaker(H1.Hand, H2.Hand);
   end Alt_Weaker;
   package Alt_Hab_Sorting is new Hand_And_Bid_Vectors.Generic_Sorting
     ("<" => Alt_Weaker);

   Hand_And_Bids : Hand_And_Bid_Vector;
begin
   while not End_Of_File loop
      declare
	 S : constant String := Get_Line;
	 Hab : Hand_And_Bid;
      begin
	 Parse_Hand_And_Bid(S, Hab);
	 Hand_And_Bids.Append(Hab);
      end;
   end loop;
   
   declare
      Sum_Winnings : Integer := 0;
   begin
      Hab_Sorting.Sort(Hand_And_Bids);
      for I in Hand_And_Bids.First_Index .. Hand_And_Bids.Last_Index loop
	 -- Put_Line(String(Hand_And_Bids(I).Hand));
	 Sum_Winnings := Sum_Winnings + Hand_And_Bids(I).Bid * I;
      end loop;
      Put_Line(Integer'Image(Sum_Winnings));
   end;
   
   declare
      Sum_Winnings : Integer := 0;
   begin
      Alt_Hab_Sorting.Sort(Hand_And_Bids);
      for I in Hand_And_Bids.First_Index .. Hand_And_Bids.Last_Index loop
	 -- Put_Line(String(Hand_And_Bids(I).Hand));
	 Sum_Winnings := Sum_Winnings + Hand_And_Bids(I).Bid * I;
      end loop;
      Put_Line(Integer'Image(Sum_Winnings));
   end;

end Advent_07;
