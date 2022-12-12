with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Advent_05_2 is
  type Stack_ID is range 1 .. 9;
  type Stack_Element_Array is array (1 .. 200) of Character; -- should be enough
  type Stack is record
    Elements: Stack_Element_Array;
    Last: Natural := 0;
  end record;

  procedure Push(S: in out Stack; C: Character) is
  begin
    S.Last := S.Last + 1;
    S.Elements(S.Last) := C;
  end Push;

  function Top(S: in Stack) return Character is
  begin
    return S.Elements(S.Last);
  end Top;

  procedure Pop(S: in out Stack) is
  begin
    S.Last := S.Last - 1;
  end Pop;

  type Stack_Array is array (Stack_ID) of Stack;
  type Stacks_Of_Crates is record
    Num_Stacks: Stack_ID := 1;
    Stacks: Stack_Array;
  end record;

  procedure Read_Puzzle(Puzzle: out Stacks_Of_Crates) is
    procedure Insert_Inner(S: in out Stack; C: Character) is
    begin
      if S.Last = 0 then
        Push(S, C);
      else
        S.Elements(2 .. S.Last + 1) := S.Elements(1 .. S.Last);
	S.Elements(1) := C;
	S.Last := S.Last + 1;
      end if;
    end Insert_Inner;
  begin
    loop
      declare
        S: constant String := Get_Line;
      begin
        -- Last line is " 1   2 ..."
        exit when S(2) = '1';
	for I in Stack_ID'Range loop
	  declare
	    Idx: constant Positive := Positive((I - 1) * 4 + 2);
	  begin
	    if Idx <= S'Last and then S(Idx) /= ' ' then
	      Puzzle.Num_Stacks := Stack_ID'Max(Puzzle.Num_Stacks, I);
	      Insert_Inner(Puzzle.Stacks(I), S(Idx));
	    end if;
	  end;
	end loop;
      end;
    end loop;
  end Read_Puzzle;

  procedure Move_Crates(Src_Stack, Dst_Stack: in out Stack; Num: Positive) is
  begin
    Dst_Stack.Elements(Dst_Stack.Last + 1 .. Dst_Stack.Last + Num) :=
      Src_Stack.Elements(Src_Stack.Last - Num + 1 .. Src_Stack.Last);
    Src_Stack.Last := Src_Stack.Last - Num;
    Dst_Stack.Last := Dst_Stack.Last + Num;
  end Move_Crates;

  Puzzle: Stacks_Of_Crates;
begin
  Read_Puzzle(Puzzle);
  declare
    Unused_S : constant String := Get_Line;
  begin
    null;
  end;

  while not End_Of_File loop
    declare
      Cmd : constant String := Get_Line;
      Last : Positive;
      Move_Num, From, To: Positive;
    begin
      Get(Cmd(Cmd'First + 5 .. Cmd'Last), Move_Num, Last);
      Get(Cmd(Last + 7 .. Cmd'Last), From, Last);
      Get(Cmd(Last + 5 .. Cmd'Last), To, Last);

      declare
        Src_Stack: Stack renames Puzzle.Stacks(Stack_ID(From));
        Dst_Stack: Stack renames Puzzle.Stacks(Stack_ID(To));
      begin
        Move_Crates(Src_Stack, Dst_Stack, Move_Num);
      end;
    end;
  end loop;

  for I in 1 .. Puzzle.Num_Stacks loop
    Put(Top(Puzzle.Stacks(I)));
  end loop;
end Advent_05_2;