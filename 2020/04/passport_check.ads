package passport_check is
	function Is_Valid_Birth_Year(s: String) return Boolean;
	function Is_Valid_Issue_Year(s: String) return Boolean;
	function Is_Valid_Expiration_Year(s: String) return Boolean;
	function Is_Valid_Height(s: String) return Boolean;
	function Is_Valid_Hair_Color(s: String) return Boolean;
	function Is_Valid_Eye_Color(s: String) return Boolean;
	function Is_Valid_Passport_ID(s: String) return Boolean;
private
	function Is_Valid_Num_String(s: String; Len: Positive) return Boolean;
	function Is_Valid_Hex_String(s: String; Len: Positive) return Boolean;
	function Is_Valid_Year(s: String) return Boolean is (Is_Valid_Num_String(s, 4));
end passport_check;
