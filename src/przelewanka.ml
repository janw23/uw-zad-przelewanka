open Array

(* Moduł do szybkiego wyszukiwania ciągów liczb całkowitych w mapie *)
module IntSequence = struct
	type t = int array
	let compare_index = ref 0

	(* Porównuje elementy usprawniając wyszukiwanie imperatywnie *)
	let compare a b =
		print_int !compare_index;
		print_string "\n";
		assert (length a = length b);
		let rec cmp a b =
			match Stdlib.compare a.(!compare_index) b.(!compare_index) with
			| 0 -> begin
				if !compare_index = length a - 1 then 0
				else (compare_index := !compare_index + 1; cmp a b) end
			| c -> c
		in cmp a b

	(* Po znalezieniu elementu należy  *)
	let compare_reload (x : unit) =
		compare_index := 0
end

(* Moduł przechowujący stany, gdzie kluczem jest tablica z ilością wody w wiaderkach *)
module StateHolder = Map.Make(IntSequence);;

(* Operacje na stateholderze, takie jak na mapie *)
let stateFind key stateHolder = 
	IntSequence.compare_reload ();
	StateHolder.find key stateHolder

let stateAdd key value stateHolder = 
	IntSequence.compare_reload ();
	StateHolder.add (copy key) value stateHolder

let stateMem key stateHolder = 
	IntSequence.compare_reload ();
	StateHolder.mem key stateHolder

let max a b = if a > b then a else b
and min a b = if a < b then a else b

let first  (a, _) = a
and second (_, b) = b

(* Pojemność i docelowa ilość wody w szklankach i liczba szklanek *)
let capacity = ref [||]
and target = ref [||]
and glassCount = ref 0

(* Operacje dostępne do wykonania na szklankach *)
let op_fill state index =
	set state index (!capacity).(index)

let op_drain state index =
	set state index 0

let op_transfer state from too =
	let avail = capacity.(too) - state.(too) in
	set state too (min cap.(too) (state.(from) + state.(too)));
	set state from (max 0 (state.(from) - avail))  

(* Bada konkretny, wygenerowany stan *)
let checkState stateHolder state cost isFinal =
	if not stateMem state stateHolder then begin
		if isFinal state then raise (FinalFound cost)
		else stateAdd state (cost + 1) stateHolder
	end

(* Generuje nowe stany do zbadania *)
let generateStates stateHolder isFinal origin cost =
	let newcost = cost + 1
	and state = copy origin in begin
		let prev_val = ref 0 in begin
			(* Generuje stan, sprawdza go i przywraca początkową wartość *)
			for i = 0 to glassCount - 1 do
				prev_val := state.(i);
				op_fill state i;
				checkState stateHolder state newcost isFinal;
				set state i !prev_val;
			done;

			for i = 0 to glassCount - 1 do
				prev_val := state.(i);
				op_drain state i;
				checkState stateHolder state newcost isFinal;
				set state i !prev_val;
			done;
		end;
		let prev_val = ref (0, 0) in begin
			for i = 0 to glassCount - 1 do
				for j = 0 to glassCount - 1 do
					if i <> j then begin
						prev_val := (state.(i), state(j));
						op_transfer state i j;
						checkState stateHolder state newcost isFinal;
						set state i (first !prev_val);
						set state j (second !prev_val);
					end
				done
			done
		end
	end

(* data - tablica par (początkowa, docelowa) ilość wody *)
(* Rozwiązanie brute force							    *)
let przelewanka (data : (int * int) array) =
	glassCount := length data;
	capacity := make !glassCount 0;
	target := make !glassCount 0;

	for i = 0 to glassCount - 1 do
		set !capacity i (first data.(i));
		set !target i (second data.(i))
	done;

	let isFinal state = begin
		let cmp x = 
			for i = 0 to !glassCount - 1 do
				if state.(i) <> (!target).(i) then raise Different
			done;
		in try (cmp (); true) with Different -> false
	end in
	

