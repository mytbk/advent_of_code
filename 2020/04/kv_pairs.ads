package kv_pairs is
	function get_key(kvstr: String) return String;
	function get_value(kvstr: String) return String;
	function get_non_spaced_string(str: String; lastPos: out Positive) return String;
end kv_pairs;
