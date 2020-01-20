let przelewanka data = 5

let stateHolder = Hashtbl.create 100000000;;

let tab = [1; 2; 3];;

Hashtbl.add stateHolder tab 5;
Hashtbl.add stateHolder [5; 6; 8] 10;;

let hashtbl_string tbl key =
	try (string_of_int (Hashtbl.find tbl key))
	with Not_found -> "NotFound";;

print_string (hashtbl_string stateHolder [1; 2; 3]);
print_string (hashtbl_string stateHolder [3; 5; 2]);
print_string (hashtbl_string stateHolder [5; 6; 8]);
;;
let str_list lst = 
	let rec helper l acc =
		match l with
		| [] -> acc ^ "]"
		| h :: [] -> acc ^ (string_of_int h) ^ "]"
		| h :: t -> helper t (acc ^ (string_of_int h) ^ "; ")
	in helper lst "[";;

let str_array arr = 
	let acc = ref "[" in begin
		for i = 0 to Array.length arr - 1 do
			acc := !acc ^ (string_of_int arr.(i)) ^ "; ";
		done;
		!acc ^ "]"
	end

let str_pair a b = 
	(str_list a) ^ " -> " ^ (string_of_int b);; 

Hashtbl.iter (fun key -> (fun v -> print_string ((str_pair key v) ^ "\n"))) stateHolder;;