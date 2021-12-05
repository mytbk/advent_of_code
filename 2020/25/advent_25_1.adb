with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure advent_25 is
	type KeyType is mod 20201227;
	subtype LoopSize is Integer range 0 .. 20201226;

	function compute_shared_key(peer_pub : KeyType; my_priv: LoopSize) return KeyType is
		square_val : KeyType;
		res0: KeyType;
	begin
		if my_priv = 0 then
			return 1;
		end if;
		square_val := peer_pub * peer_pub;
		res0 := compute_shared_key(square_val, my_priv / 2);
		if my_priv mod 2 /= 0 then
			return res0 * peer_pub;
		else
			return res0;
		end if;
	end compute_shared_key;

	generator : constant KeyType := 7;

	function guess_priv(pubkey: KeyType) return LoopSize is
		expt: LoopSize := 0;
		curpub: KeyType := 1;
	begin
		while curpub /= pubkey loop
			curpub := curpub * generator;
			expt := expt + 1;
		end loop;
		return expt;
	end guess_priv;

	Input_Pub1, Input_Pub2 : KeyType;
	Priv1, Priv2: LoopSize;
	package KeyType_IO is new Modular_IO(Num => KeyType);
	use KeyType_IO;
begin
	Get(Input_Pub1);
	Get(Input_Pub2);
	Priv1 := guess_priv(Input_Pub1);
	Priv2 := guess_priv(Input_Pub2);
	Put("Priv1 is "); Put(Priv1); New_Line;
	Put("Priv2 is "); Put(Priv2); New_Line;
	Put(compute_shared_key(Input_Pub2, Priv1));
end advent_25;
