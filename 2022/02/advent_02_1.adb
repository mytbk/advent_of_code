with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure advent_02_1 is
  type Shape is (Rock, Paper, Scissors);
  -- Score_Table (opponent, self) is the score for a round
  Score_Table: constant array (Shape, Shape) of Natural :=
    (Rock => (Rock => 1 + 3, Paper => 2 + 6, Scissors => 3 + 0),
     Paper => (Rock => 1 + 0, Paper => 2 + 3, Scissors => 3 + 6),
     Scissors => (Rock => 1 + 6, Paper => 2 + 0, Scissors => 3 + 3));

  function Get_Score(S: String) return Natural is
    opponent: Shape;
    myself: Shape;
  begin
    case S(S'First) is
      when 'A' => opponent := Rock;
      when 'B' => opponent := Paper;
      when 'C' => opponent := Scissors;
      when others => raise Constraint_Error;
    end case;

    case S(S'First + 2) is
      when 'X' => myself := Rock;
      when 'Y' => myself := Paper;
      when 'Z' => myself := Scissors;
      when others => raise Constraint_Error;
    end case;

    return Score_Table(opponent, myself);
  end;

  Total_Score: Natural := 0;
begin
  while not End_Of_File loop
    Total_Score := Total_Score + Get_Score(Get_Line);
  end loop;

  Put(Total_Score);
  New_Line;
end advent_02_1;