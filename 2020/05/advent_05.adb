with Ada.Text_IO;
use Ada.Text_IO;

procedure advent_05 is
	symbol_to_bin_map : constant array (Character) of Character :=
		('F' => '0', 'B' => '1', 'L' => '0', 'R' => '1', others => ' ');

	subtype SeatID is Natural range 0 .. (2 ** 10 - 1);
	type SeatSpec is new String(1 .. 10);

	function seat_spec_to_id(s: SeatSpec) return SeatID is
		converted: String(1 .. 10);
	begin
		for I in s'Range loop
			converted(I) := symbol_to_bin_map(s(I));
		end loop;
		return SeatID'Value("2#" & converted & "#");
	end seat_spec_to_id;

	maxid: SeatID := SeatID'First;
	id_in_list: array (SeatID) of Boolean := (others => False);
begin
	while not End_Of_File loop
		declare
			spec : constant String := Get_Line;
			sid : SeatID := seat_spec_to_id(SeatSpec(spec(spec'First .. spec'First + 9)));
		begin
			id_in_list(sid) := True;
			if sid > maxid then
				maxid := sid;
			end if;
		end;
	end loop;

	Put(SeatID'Image(maxid));

	for I in (SeatID'First + 1) .. (SeatID'Last - 1) loop
		if (not id_in_list(I)) and then id_in_list(I - 1) and then id_in_list(I + 1) then
			Put(SeatID'Image(I));
		end if;
	end loop;
end advent_05;
