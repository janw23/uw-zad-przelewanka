open Array

exception FoundSolution of int

(* Tablica haszująca, w która mapuje stan napełnienia kubków *)
(* do najmniejszej liczby operacji potrzebnych do uzyskania go *)
let stateHolder = Hashtbl.create 1000000(*100000000*)
and states_to_add = ref [] (* Stany oczekujące na dodanie do stateHoldera 	 *)
						   (* Nie mogą zostać dodane od razu po wygenerowaniu *)
						   (* bo zepsułoby to działanie hashtbl 				 *)

let capacity = ref [||]
and target = ref [||]
and state_size = ref 0

(* Funkcje pomocnicze *)
let max a b = if a > b then a else b
and min a b = if a < b then a else b

let first  (a, _) = a
and second (_, b) = b

(* ----Operacje, jakie można wykonać na kubkach---- *)
(*let set_value (state : int list) value index =
	let rec traverse lst k =
		match lst with h :: t -> 
			if k = 0 then value :: t
			else h :: traverse t (k - 1) 
	in traverse state index *)

let fill (state : int array) index =
	let orig = state.(index) in begin
		set state index !capacity.(index);
		(fun _ -> set state index orig)
	end

let drain (state : int array) index =
	let orig = state.(index) in begin
		set state index 0;
		(fun _ -> set state index orig)
	end

let transfer (state : int array) from too =
	let orig_from = state.(from)
	and orig_too  = state.(too) in begin
		let avail = !capacity.(too) - state.(too) in
		set state too (min !capacity.(too) (state.(from) + state.(too)));
		set state from (max 0 (state.(from) - avail));
		(fun _ -> set state from orig_from; set state too orig_too)
	end

(* (* let str_array arr =  *)
	let acc = ref "[" in begin
		for i = 0 to Array.length arr - 1 do
			acc := !acc ^ (string_of_int arr.(i)) ^ "; ";
		done;
		!acc ^ "]"
	end *)

(* Sprawdza, czy nowy stan jest rozwiązaniem *)
(* Jeśli nim nie jest, to sprawdza, czy wcześniej wystąpił *)
(* Jeśli nie, to dodaje go na stos stanów do dodania po zakończeniu generowania *)
let propose_state state cost =
	if state = !target then raise (FoundSolution cost)
	else if not (Hashtbl.mem stateHolder state) then
		states_to_add := ((Array.copy state), cost) :: !states_to_add

let generateStates state cost =
	let newcost = cost + 1 in begin
		for i = 0 to !state_size - 1 do
			let revert = fill state i in
				propose_state state newcost;
				revert ();
		done;
		for i = 0 to !state_size - 1 do
			let revert = drain state i in
				propose_state state newcost;
				revert ();
		done;
		for i = 0 to !state_size - 1 do
			for j = 0 to !state_size - 1 do
				if j <> i then begin
					let revert = transfer state i j in
					propose_state state newcost;
					revert ();
				end
			done
		done
	end

let add_proposed_states _ = 
	(* print_string ("    stateHolder length = " ^ (string_of_int (Hashtbl.length stateHolder)) ^ "\n"); *)
	let any_added = ref false in
	let rec iterate lst =
		match lst with
		| [] -> ()
		| (state, cost) :: t -> begin
			if not (Hashtbl.mem stateHolder state) then 
				(Hashtbl.add stateHolder state cost; any_added := true);
			iterate t
		end
	in iterate !states_to_add;
	states_to_add := [];
	if not !any_added then raise (FoundSolution (-1))

(* let println str =  *)
	(* print_string (str ^ "\n") *)

let test_counter = ref 0

let run_przelewanka data =
	print_string ("running on test #" ^ (string_of_int !test_counter) ^ "\n");
	test_counter := !test_counter + 1;
	flush_all ();
	(* Resetowanie struktur przed ponownym uruchomieniem *)
	Hashtbl.reset stateHolder;
	states_to_add := [];

	(* Inicjowanie potrzebnych struktur *)
	state_size := length data;
	capacity := make !state_size 0;
	target := make !state_size 0;

	for i = 0 to !state_size - 1 do
		set !capacity i (first  data.(i));
		set !target   i (second data.(i))
	done;

	(* Dodawanie stanu początkowego *)
	propose_state (make !state_size 0) 0;

	while true do
		Hashtbl.iter generateStates stateHolder;
		add_proposed_states ();
	done;;

let przelewanka data =
	try (run_przelewanka data; -1)
	with FoundSolution c -> c;; 

(* print_int (przelewanka [|(0, 1); (0, 2); (0, 3)|]);; *)


(* capacity := [|1; 2; 3; 4; 5|];;

let state = [|0; 2; 2; 1; 4|];;

println (str_array state);;
let revert = transfer state 2 0;;
println (str_array state);;
revert ();;
println (str_array state);; *)