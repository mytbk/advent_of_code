with Ada.Containers.Vectors;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure advent_09 is
	type MyInt is range 0 .. 10 ** 14;
	package IVecs is new Ada.Containers.Vectors(
		Element_Type => MyInt,
		Index_Type => Natural);
	subtype IVec is IVecs.Vector;

	function find_first_invalid_idx(numbers: IVec; preamble_len: Positive) return IVecs.Extended_Index is
		first_idx : constant Natural := IVecs.First_Index(numbers);
		valid : Boolean;
	begin
		for I in first_idx + preamble_len .. IVecs.Last_Index(numbers) loop
			valid := False;
			for J in (I - preamble_len) .. (I - 1) loop
				exit when valid;
				for K in (J + 1) .. (I - 1) loop
					exit when valid;
					if numbers(J) /= numbers(K) and then numbers(J) + numbers(K) = numbers(I) then
						valid := True;
					end if;
				end loop;
			end loop;
			if not valid then
				return I;
			end if;
		end loop;
		return IVecs.No_Index;
	end find_first_invalid_idx;

	function find_contiguous_range_to_sum(numbers: IVec; sum: MyInt; first,last: out Natural) return Boolean is
		s: MyInt;
	begin
		for I in IVecs.First_Index(numbers) .. IVecs.Last_Index(numbers) loop
			s := 0;
			for J in I .. IVecs.Last_Index(numbers) loop
				s := s + numbers(J);
				exit when s > sum;
				if s = sum then
					first := I;
					last := J;
					return True;
				end if;
			end loop;
		end loop;
		return False;
	end find_contiguous_range_to_sum;

	numbers: IVec;
	idx: Integer;

	package MyIntIO is new Integer_IO(Num => MyInt);
	use MyIntIO;

	-- part 2
	first_idx, last_idx : Natural;
	smallest, largest: MyInt;
begin
	while not End_Of_File loop
		declare
			num: MyInt;
			L: String := Get_Line;
			Unused_Last: Positive;
		begin
			Get(L, num, Unused_Last);
			numbers.Append(num);
		end;
	end loop;

	idx := find_first_invalid_idx(numbers, 25);
	if idx = IVecs.No_Index then
		Put_Line("Not found");
	else
		Put_Line(MyInt'Image(numbers(idx)));
	end if;

	if find_contiguous_range_to_sum(numbers, numbers(idx), first_idx, last_idx) then
		smallest := numbers(first_idx);
		largest := numbers(first_idx);
		for I in first_idx + 1 .. last_idx loop
			if numbers(I) > largest then
				largest := numbers(I);
			end if;
			if numbers(I) < smallest then
				smallest := numbers(I);
			end if;
		end loop;
		Put_Line(MyInt'Image(smallest + largest));
	else
		Put_Line("Not found");
	end if;

end advent_09;
