with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Advent_04 is
  type Section is record
    Min: Integer;
    Max: Integer;
  end record;

  procedure Read_Sections(Sect1, Sect2: out Section; S: String) is
    Last: Positive;
  begin
    Get(S, Sect1.Min, Last);
    Get(S(Last + 2 .. S'Last), Sect1.Max, Last);
    Get(S(Last + 2 .. S'Last), Sect2.Min, Last);
    Get(S(Last + 2 .. S'Last), Sect2.Max, Last);
  end Read_Sections;

  function One_Contain_The_Other(Sect1, Sect2: Section) return Boolean is
  begin
    if Sect1.Min < Sect2.Min then
      return Sect1.Max >= Sect2.Max;
    elsif Sect1.Min > Sect2.Min then
      return Sect1.Max <= Sect2.Max;
    else
      return True;
    end if;
  end One_Contain_The_Other;

  function Overlaps(Sect1, Sect2: Section) return Boolean is
  begin
    return not (Sect1.Max < Sect2.Min or Sect1.Min > Sect2.Max);
  end Overlaps;

  Num_Contains : Natural := 0;
  Num_Overlaps : Natural := 0;
begin
  while not End_Of_File loop
    declare
      Sect1, Sect2: Section;
    begin
      Read_Sections(Sect1, Sect2, Get_Line);
      if One_Contain_The_Other(Sect1, Sect2) then
        Num_Contains := Num_Contains + 1;
      end if;
      if Overlaps(Sect1, Sect2) then
        Num_Overlaps := Num_Overlaps + 1;
      end if;
    end;
  end loop;
  -- part 1
  Put(Num_Contains);
  New_Line;
  -- part 2
  Put(Num_Overlaps);
  New_Line;
end Advent_04;