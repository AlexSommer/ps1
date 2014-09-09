open String
(* Precondition: takes an integer list
 * Postcontition: returns a boolean that is true if the list is 
 * monotonically increasing and flase otherwise  *)
let rec is_mon_inc (l: int list) : bool = 
	match l with 
	| []-> true 
	| [x] -> true 
	| h::h2::t -> if h <= h2 then is_mon_inc(h2::t) else false

(* Precondition: takes an integer list 
 * Postcondition: returns a boolean that is true if the list is
 * monotonically decreasing, false otherwise *)
let rec is_mon_dec (l : int list) : bool =
	match l with
	| [] -> true
	| [x] -> true
	| h::h2::t -> if h>=h2 then is_mon_dec(h2::t) else false

(* Precondition: takes an integer list 
 * Postcondition: returns a boolean that is true if the list is
 * unimodal, false otherwise *)
let rec is_unimodal (l : int list) : bool =
	match l with
	| [] -> true
	| [x] -> true
	| h::h2::t -> if h <= h2 then is_unimodal(h2::t) else is_mon_dec(h2::t)

(* Precondition: takes an integer x and a powerset, which is a list of int list 
 * Postcondition: concatinates x onto each element in the powerset *)
let rec concatEls (x : int) (pset: int list list) : int list list =
	match pset with 
	| [] -> [[x]]
	| [y] -> [x::y]
	| h::t -> (x::h) :: (concatEls x t )

(* Precondition: takes an integer list 
 * Postcondition: returns the powerset of the set of integers in the list *)
let rec powerset (l: int list): int list list = 
	match l with
	| [] -> [[]]
	| h::t -> powerset(t) @ (concatEls h (powerset(t)))

(* Precondition: takes two integer values 
 * Postcondition: returns an integer value equivalent to
 * the first int raised to the power of the second int *)
let rec power (base : int) (raised : int) : int =
	if raised <= 0 then 1 else base * (power base (raised-1))

(* Precondition: takes an integer value lower than Max_Int 
 * Postcondition: returns the integer with the didgets of the
 * input reversed *)
let rec rev_int (value : int) : int =
	if value = 0 then 0 else 
		let first = value mod 10 in
		let len = length (string_of_int (abs (value))) in
		(first * (power 10 (len-1))) + (rev_int (value / 10))


					
(* Precondition: takes an integer value lower than Max_Int 
 * Postcondition: returns the integer with the didgets of the
 * input reversed *)
let rec build_subset (k: int) (lst : 'a list)  (temp : 'a list) : 'a list * 'a list =
	match lst with
	| [] -> ([],[]) (* will never be called *)
	| h :: t -> 
		let result = 
			if (k>1) then (build_subset (k-1) (t) (temp @ [h])) else
			((temp @ [h]) , t)
		in result

(* Precondition: takes an list of type 'a and a integer k
 * Postcondition: returns a list of type 'a list that contains the origional
 * list broken up into subsets of size k. If k does not divide evenly, the last list 
 * will contain the rest of the origional list, which is the size of the remainder
 * the list length / k *)
let rec unflatten (k : int) (l : 'a list) : 'a list list option = 
	if (k <= 0) then None
	else if (List.length l <= k) then Some [l] 
	else 
		let (x, y) = (build_subset k l []) in
		if ((List.length y) > k) then 
			let rest = match (unflatten k y) with
				| None -> [[]]
				| Some c -> c  in	
			Some ( x :: rest)
		else 
			Some ( [x] @ [y] ) 
		 




type numeral = I | V | X | L | C | D | M

type roman = numeral list

(* Precondition: takes an value of type Roman that must be valid as defined 
 * by the problem
 * Postcondition: returns the integer equivilent of the roman *)
let rec int_of_roman ( r : roman ) : int =
	let int_of_numeral = function
		| I -> 1
		| V -> 5
		| X -> 10
		| L -> 50
		| C -> 100
		| D -> 500
		| M -> 1000 in
	match r with
	| [] -> 0
	| [x] -> int_of_numeral x
	| h1 :: h2 :: tl -> if (int_of_numeral h1)>=(int_of_numeral h2) then
	(int_of_numeral h1) + (int_of_roman (h2::tl)) else
	~-(int_of_numeral h1) + (int_of_roman (h2::tl))
