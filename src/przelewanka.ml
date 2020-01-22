(* Zadanie Przelewanka 				*)
(* Autor: Jan Wawszczak  			*)
(* Code review: Szymon Frąckowiak 	*)

open Array

exception FoundSolution of int

(* Tablica haszująca, która mapuje stan napełnienia kubków *)
(* do najmniejszej liczby operacji potrzebnych do uzyskania go *)
let stateHolder = Hashtbl.create 5000000

(* Kolejka stanów oczekujących na odwiedzenie BFSem *)
and states_que = ref (Queue.create ())

let capacity = ref [||]	(* Pojemność kubków 	*)
and target = ref [||]	(* Docelowa ilość wody 	*)
and state_size = ref 0	(* Liczba kubków 		*)

(* Funkcje pomocnicze *)
let max a b = if a > b then a else b
and min a b = if a < b then a else b

let first  (a, _) = a
and second (_, b) = b

(* Oblicza NWD liczb [a] i [b] 	*)
(* Warunek: a > 0, b >= 0 		*)
let rec gcd a b =
	if b = 0 then a
	else gcd b (a mod b) 

(* ----Operacje, jakie można wykonać na kubkach---- *)
(* 	   Każda operacja modyfikuje podany stan  		*)
(* 	   oraz zwraca funkcję, która po wywołaniu 		*)
(* 	   przywraca wartości stanu do tych 			*)
(*	   sprzed wykonania operacji 					*)

(* Napełnienie kubka na pozycji [index] *)
let fill (state : int array) index =
	let orig = state.(index) in begin
		set state index !capacity.(index);
		(fun _ -> set state index orig)
	end

(* Opróżnienie kubka na pozycji [index] *)
let drain (state : int array) index =
	let orig = state.(index) in begin
		set state index 0;
		(fun _ -> set state index orig)
	end

(* Przelanie zawartości kubka na pozycji [from] *)
(* do kubka na pozycji [too]				 	*)
let transfer (state : int array) from too =
	let orig_from = state.(from)
	and orig_too  = state.(too) in begin
		let avail = !capacity.(too) - state.(too) in
		set state too (min !capacity.(too) (state.(from) + state.(too)));
		set state from (max 0 (state.(from) - avail));
		(fun _ -> set state from orig_from; set state too orig_too)
	end

(* Sprawdza, czy zaproponowany stan już wcześniej wystąpił 	*)
(* Jeśli nie, to sprawdza, czy jest on rozwiązaniem		 	*)
(* Jeśli nie, to oznacza jego rozpatrzenie 					*)
(* i dodaje go do kolejki, aby później odwiedzić go BFSem 	*)
let propose_state state cost =
	if not (Hashtbl.mem stateHolder state) then begin
		if state = !target then raise (FoundSolution cost);
		Hashtbl.add stateHolder (copy state) cost;
		Queue.push (copy state, cost) !states_que
	end

(* Generuje i sprawdza nowe stany, które mogą powstać bezpośrednio 	*)
(* z pojedyńczych operacji wykonanych na stanie wejściowym 			*)
let generateStates state cost =
	let newcost = cost + 1 in begin
		for i = 0 to !state_size - 1 do
			let revert = fill state i in begin
				propose_state state newcost;
				revert ();
			end;

			let revert = drain state i in begin
				propose_state state newcost;
				revert ();
			end;

			for j = 0 to !state_size - 1 do if j <> i then
				let revert = transfer state i j in begin
					propose_state state newcost;
					revert ();
				end;
			done
		done
	end

(* Przechodzi przez wszystkie stany w poszukiwaniu rozwiązania 	*)
(* zaczynając od stanu [originState] o nadanym koszcie 0 		*)
let bfs originState =
	states_que := Queue.create ();
	propose_state originState 0;

	while not (Queue.is_empty !states_que) do
		let state, cost = Queue.pop !states_que in
		generateStates state cost;
	done

(* Sprawdza, czy zachodzi wymagany warunek podzielności *)
(* docelowych ilości wody przez NWD pojemnościu kubków  *)
let check_gcd_condition computed_gcd =
	if computed_gcd = 0 then raise (FoundSolution (-1)) else
		for i = 0 to !state_size - 1 do
			if !target.(i) mod computed_gcd <> 0 then raise (FoundSolution (-1))
		done

let run_przelewanka data =
	if length data = 0 then raise (FoundSolution 0);

	(* Resetowanie struktur przed ponownym uruchomieniem *)
	Hashtbl.reset stateHolder;

	(* Inicjowanie potrzebnych struktur *)
	state_size := length data;
	capacity := make !state_size 0;
	target := make !state_size 0;

	let any_full_or_empty = ref false
	and computed_gcd = ref 0 in

	for i = 0 to !state_size - 1 do
		set !capacity i (first  data.(i));
		set !target   i (second data.(i));

		if !target.(i) = 0 || !target.(i) = !capacity.(i)
			then any_full_or_empty := true;

		(* Obliczanie NWD niezerowych pojemności kubków *)
		if !capacity.(i) > 0 then begin
			if !computed_gcd = 0 then computed_gcd := !capacity.(i)
			else computed_gcd := gcd !computed_gcd !capacity.(i) 
		end
	done;

	(* Sprawdzanie, czy docelowy stan to taki, gdzie wszystkie kubki nie mają wody *)
	if !target = make !state_size 0 then raise (FoundSolution 0);

	(* Sprawdzanie, czy warunki konieczne dot. końcowej ilości wody są spełnione *)
	if not !any_full_or_empty then raise (FoundSolution (-1));
	check_gcd_condition !computed_gcd;

	(* W przypadku braku oczywistej odpowiedzi, przeszukiwanie przestrzeni stanów BFSem *)
	try (bfs (make !state_size 0); -1)
	with FoundSolution c -> c

let przelewanka data =
	try (run_przelewanka data)
	with FoundSolution c -> c
;;

(* Testy *)
(*
(*Nie ma rozwiązania*)
let c = [|(10,2);(20,20);(10,0);(1000,1000)|];;
assert ( przelewanka c = -1 );;
let c = [|(3,2);(5,4);(5,2);(6,1)|];;
assert (przelewanka c = -1);;
let c = [|(40,1);(10,4);(23,2);(40,1)|];;
assert (przelewanka c = -1);;
let c = [|(12,2);(6,3);(4,4);(10,2)|];;
assert (przelewanka c = -1);;
let c = [|(14,3);(3,1)|];;
assert (przelewanka c = -1);;

(*Testy różne*)
let c = [|(3,2);(3,3);(1,0);(12,1)|];;
assert ( przelewanka c = 4 );;
let c = [|(1,1);(100,99)|];;
assert ( przelewanka c = 2 );;
let c = [|(3,3);(5,4);(5,2);(6,1)|];;
assert (przelewanka c = 6);;
let c = [|(100,3);(2,1);(1,0);(6,1)|];;
assert (przelewanka c = 7);;
let c = [|(3,3);(5,5);(5,5);(6,6)|];;
assert (przelewanka c = 4);;
let c = [|(40,20);(20,10);(10,5);(5,0)|];;
przelewanka c ;;
let c = [|(19,3);(1,1);(2,2)|];;
assert (przelewanka c = 6);;
let c = [|(14,3);(3,1);(3,0)|];;
assert (przelewanka c = 13);;
let c = [|(3,3);(4,0);(1,1);(6,6)|];;
assert (przelewanka c = 3);;
let c = [|(46,20);(23,10);(13,5);(5,0)|];;
assert (przelewanka c = 10);;
let c = [|(18,3);(3,1);(2,2)|];;
assert (przelewanka c = 4);;
let c = [|(14,3);(5,1)|];;
assert (przelewanka c = -1);;
let c = [|(14,3);(5,1);(5,0)|];;
assert (przelewanka c = 16);;

(* Przelewanie ciągle z jednego do drugiego*)
let c = [|(10000,5000);(1,0)|];;
assert (przelewanka c = 10000);;
let c = [|(50000,450);(3,1);(3,0)|];;
assert (przelewanka c = 33635);;
let c = [|(100000,25252);(2,2)|];;
assert (przelewanka c = 25253);;
*)