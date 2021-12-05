package Passport_Processing is
	type Passport_Data_Info is record
		Has_Birth_Year : Boolean;
		Has_Issue_Year : Boolean;
		Has_Expiration_Year : Boolean;
		Has_Height : Boolean;
		Has_Hair_Color : Boolean;
		Has_Eye_Color : Boolean;
		Has_Passport_ID : Boolean;
		Has_Country_ID : Boolean;
	end record;

	function Is_Valid_Passport(p: Passport_Data_Info) return Boolean is
		(p.Has_Birth_Year and then p.Has_Issue_Year and then p.Has_Expiration_Year and then
		 p.Has_Height and then p.Has_Hair_Color and then p.Has_Eye_Color and then
		 p.Has_Passport_ID);

end Passport_Processing;
