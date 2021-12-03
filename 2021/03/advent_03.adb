-- TODO: very rough version, needs refactoring

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure advent_03 is
	package Report_Vectors is new Ada.Containers.Vectors(Element_Type => Unbounded_String, Index_Type => Natural);
	subtype ReportVec is Report_Vectors.Vector;

	function power_consumption(reports: ReportVec) return Integer is
		BitLen : constant Integer := Length(reports(0));
		gamma, epsilon: String(1 .. BitLen);

		gamma_num, epsilon_num: Integer;
		Unused_Last: Positive;

		type Freq_Array is array (1 .. BitLen) of Natural;
		num_one, num_zero: Freq_Array := (others => 0);
	begin
		for r of reports loop
			for I in 1 .. BitLen loop
				if To_String(r)(I) = '0' then
					num_zero(I) := num_zero(I) + 1;
				else
					num_one(I) := num_one(I) + 1;
				end if;
			end loop;
		end loop;

		for I in 1 .. BitLen loop
			if num_one(I) > num_zero(I) then
				gamma(I) := '1';
				epsilon(I) := '0';
			else
				gamma(I) := '0';
				epsilon(I) := '1';
			end if;
		end loop;

		Get("2#" & String(gamma) & "#", gamma_num, Unused_Last);
		Get("2#" & String(epsilon) & "#", epsilon_num, Unused_Last);
		return gamma_num * epsilon_num;
	end power_consumption;

	function life_rating(reports: ReportVec) return Integer is
		use Ada.Containers;

		BitLen : constant Integer := Length(reports(0));
		num_one, num_zero: Natural;

		oxygen_numbers : ReportVec := reports;
		co2_numbers : ReportVec := reports;
		vec_idx : Natural;

		bitIdx : Positive;
		char_sel: Character;

		o2rate, co2rate : Integer;
		Unused_Last: Positive;
	begin
		bitIdx := 1;
		while oxygen_numbers.Length > 1 loop
			num_zero := 0;
			num_one := 0;
			for r of oxygen_numbers loop
				if To_String(r)(bitIdx) = '0' then
					num_zero := num_zero + 1;
				else
					num_one := num_one + 1;
				end if;
			end loop;

			if num_zero > num_one then
				char_sel := '0';
			else
				char_sel := '1';
			end if;
			vec_idx := oxygen_numbers.First_Index;
			while vec_idx <= oxygen_numbers.Last_Index loop
				if To_String(oxygen_numbers(vec_idx))(bitIdx) /= char_sel then
					oxygen_numbers.Delete(vec_idx);
				else
					vec_idx := vec_idx + 1;
				end if;
			end loop;
			bitIdx := bitIdx + 1;
		end loop;

		bitIdx := 1;
		while co2_numbers.Length > 1 loop
			num_zero := 0;
			num_one := 0;
			for r of co2_numbers loop
				if To_String(r)(bitIdx) = '0' then
					num_zero := num_zero + 1;
				else
					num_one := num_one + 1;
				end if;
			end loop;

			if num_zero > num_one then
				char_sel := '1';
			else
				char_sel := '0';
			end if;
			vec_idx := oxygen_numbers.First_Index;
			while vec_idx <= co2_numbers.Last_Index loop
				if To_String(co2_numbers(vec_idx))(bitIdx) /= char_sel then
					co2_numbers.Delete(vec_idx);
				else
					vec_idx := vec_idx + 1;
				end if;
			end loop;
			bitIdx := bitIdx + 1;
		end loop;

		-- Put_Line(To_String(oxygen_numbers.First_Element));
		-- Put_Line(To_String(co2_numbers.First_Element));

		Get("2#" & To_String(oxygen_numbers.First_Element) & "#", o2rate, Unused_Last);
		Get("2#" & To_String(co2_numbers.First_Element) & "#", co2rate, Unused_Last);

		return o2rate * co2rate;
	end life_rating;

	reports: ReportVec;
begin
	while not End_Of_File loop
		declare
			L: String := Get_Line;
		begin
			reports.Append(To_Unbounded_String(L));
		end;
	end loop;

	Put(power_consumption(reports));
	Put(life_rating(reports));
end advent_03;
