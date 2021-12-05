with Ada.Text_IO;
use Ada.Text_IO;
with kv_pairs;
use kv_pairs;
with Passport_Processing;
use Passport_Processing;

procedure advent_04_1 is
	procedure Set_Passport_Data_Info(p: in out Passport_Data_Info; k: String; v: String) is
	begin
		-- Put_Line("Set passport field " & k);

		if k = "byr" then
			p.Has_Birth_Year := True;
		elsif k = "iyr" then
			p.Has_Issue_Year := True;
		elsif k = "eyr" then
			p.Has_Expiration_Year := True;
		elsif k = "hgt" then
			p.Has_Height := True;
		elsif k = "hcl" then
			p.Has_Hair_Color := True;
		elsif k = "ecl" then
			p.Has_Eye_Color := True;
		elsif k = "pid" then
			p.Has_Passport_ID := True;
		elsif k = "cid" then
			p.Has_Country_ID := True;
		end if;
	end Set_Passport_Data_Info;

	procedure Update_Passport_Data_Info(p: in out Passport_Data_Info; info: String) is
		I : Positive := info'First;
	begin
		loop
			declare
				kv : constant String := get_non_spaced_string(info(I .. info'Last), I);
				key : constant String := get_key(kv);
			begin
				Set_Passport_Data_Info(p, key, ""); -- we don't use the value yet
			end;

			exit when I = info'Last;
			while I /= info'Last loop
				I := I + 1;
				exit when info(I) /= ' ';
			end loop;
		end loop;
	end Update_Passport_Data_Info;

	passport_begins : Boolean := False;
	passport: Passport_Data_Info := (others => False);
	num_valid : Natural := 0;
begin
	while not End_Of_File loop
		declare
			str : constant String := Get_Line;
		begin
			if str'Length = 0 then
				if passport_begins then
					if Is_Valid_Passport(passport) then
						num_valid := num_valid + 1;
					end if;
					passport_begins := False;
					passport := (others => False);
				end if;
			else
				passport_begins := True;
				Update_Passport_Data_Info(passport, str);
			end if;
		end;
	end loop;

	if passport_begins then
		if Is_Valid_Passport(passport) then
			num_valid := num_valid + 1;
		end if;
	end if;

	Put_Line(Natural'Image(num_valid));
end advent_04_1;
