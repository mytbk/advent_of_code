with Ada.Text_Io; use Ada.Text_Io;
with Ada.Containers.Vectors;

procedure Monkey_Gen is
   Str_Starting : constant String := "  Starting items: ";
   Str_Operation : constant String := "  Operation: new = ";
   Str_Test : constant String := "  Test: divisible by ";
   Str_Test_True : constant String := "    If true: throw to monkey ";
   Str_Test_False : constant String := "    If false: throw to monkey ";
   
   type String_Acc is access String;
   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => String_Acc);
   subtype String_Vector is String_Vectors.Vector;
   
   All_Monkey_Names : String_Vector;
   Common_Mod : Long_Long_Integer := 1;
   
   function Starts_With(S, Sub : String) return Boolean is
   begin
      return S'Length >= Sub'Length and then
	S(S'First .. S'First + Sub'Length - 1) = Sub;
   end Starts_With;
   
   procedure Emit_Start_Items(Name: String; Items: String) is
      Cur_Pos : Positive := Items'First;
      Proc_Name : constant String := "Monkey_" & Name & "_Init";
   begin
      Put_Line("procedure " & Proc_Name & "(Monkey : in out Monkey_State) is");
      Put_Line("begin");
      for I in Items'Range loop
	 if Items(I) = ',' then
	    Put_Line("  Insert_Item(Monkey, " & Items(Cur_Pos .. I - 1) & ");");
	    Cur_Pos := I + 1;
	 end if;
      end loop;
      Put_Line("  Insert_Item(Monkey, " & Items(Cur_Pos .. Items'Last) & ");");
      Put_Line("end " & Proc_Name & ";");
   end Emit_Start_Items;
   
   procedure Emit_Operation(Name: String; Ops : String) is
      Fname : constant String := "Monkey_" & Name & "_Op";
   begin
      Put_Line("function " & Fname & "(old: Worry_Level) return Worry_Level is");
      Put_Line("begin");
      Put_Line("  return " & Ops & ";");
      Put_Line("end " & Fname & ";");
   end Emit_Operation;
   
   procedure Emit_Test_Code(Name, Test_Num, Test_True, Test_False : String) is
      Fname : constant String := "Monkey_" & Name & "_Next";
   begin
      Put_Line("function " & Fname & "(Value: Worry_Level) return Integer is");
      Put_Line("begin");
      Put_Line("  return (if Value mod " & Test_Num & " = 0 then " & Test_True &
		 " else " & Test_False & ");");
      Put_Line("end " & Fname & ";");
   end Emit_Test_Code;

   procedure Emit_One_Monkey is
      -- FIXME: release the allocated memories
      Monkey_Name : String_Acc := null;
      Start_Items : access String := null;
      Op_Str : access String := null;
      Test_Num : access String := null;
      Test_True_Res : access String := null;
      Test_False_Res : access String := null;
   begin
      while not End_Of_File loop
	 declare
	    S: constant String := Get_Line;
	 begin
	    if Starts_With(S, "Monkey ") then
	       for I in 8 .. S'Last loop
		  if S(I) = ':' then
		     Monkey_Name := new String'(S(8 .. I - 1));
		     exit;
		  end if;
	       end loop;
	    end if;

	    if Starts_With(S, Str_Starting) then
	       Start_Items := new String'(S(Str_Starting'Length + 1 .. S'Last));
	    end if;
	    
	    if Starts_With(S, Str_Operation) then
	       Op_Str := new String'(S(Str_Operation'Length + 1 .. S'Last));
	    end if;
	    
	    if Starts_With(S, Str_Test) then
	       Test_Num := new String'(S(Str_Test'Length + 1 .. S'Last));
	    end if;
	    
	    if Starts_With(S, Str_Test_True) then
	       Test_True_Res := new String'(S(Str_Test_True'Length + 1 .. S'Last));
	    end if;   
	    
	    if Starts_With(S, Str_Test_False) then
	       Test_False_Res := new String'(S(Str_Test_False'Length + 1 .. S'Last));
	    end if;
	    
	    if Monkey_Name /= null and Start_Items /= null and  Op_Str /= null and
	      Test_Num /= null and Test_True_Res /= null and Test_False_Res /= null then
	       All_Monkey_Names.Append(Monkey_Name);
	       Common_Mod := Common_Mod * Long_Long_Integer'Value(Test_Num.all);
	       Emit_Start_Items(Monkey_Name.all, Start_Items.all);
	       Emit_Operation(Monkey_Name.all, Op_Str.all);
	       Emit_Test_Code (Monkey_Name.all, Test_Num.all, Test_True_Res.all, Test_False_Res.all);
	       New_Line;
	       return;
	    end if;
	 end;
      end loop;
   end Emit_One_Monkey;
begin
   Put_Line("with Monkeys; use Monkeys;");
   Put_Line("package body Monkey_Defs is");
   
   while not End_Of_File loop
      Emit_One_Monkey;
   end loop;
   
   Put_Line("procedure Init_Monkeys is");
   Put_Line("begin");
   for I in All_Monkey_Names.First_Index .. All_Monkey_Names.Last_Index loop
      declare
	 M_Name : String renames All_Monkey_Names(I).all;
      begin
	 Put_Line("  All_Monkeys.Append(Monkey_Desc'(others => <>));");
	 Put_Line("  Monkey_" & M_Name & "_Init(All_Monkeys(" & Natural'Image(I) & ").State);");
	 Put_Line("  All_Monkeys(" & Natural'Image(I) & ").Subprogs.Operation := Monkey_" & M_Name & "_Op'Access;");
	 Put_Line("  All_Monkeys(" & Natural'Image(I) & ").Subprogs.Next_Func := Monkey_" & M_Name & "_Next'Access;");
      end;
   end loop;
   Put_Line("end Init_Monkeys;");
   
   Put_Line("function Common_Mod return Worry_Level is begin");
   Put_Line("  return " & Long_Long_Integer'Image(Common_Mod) & ";");
   Put_Line("end Common_Mod;");

   Put_Line("end Monkey_Defs;");
end Monkey_Gen;
