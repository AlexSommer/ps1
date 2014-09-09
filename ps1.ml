open String

let rec is_mon_inc (list : int list) : bool = 
	match list with 
	| []-> true 
	| [x] -> true 
	| h::h2::t -> if h <= h2 then is_mon_inc(h2::t) else false

let rec is_mon_dec (list : int list) : bool =
	match list with
	| [] -> true
	| [x] -> true
	| h::h2::t -> if h>=h2 then is_mon_dec(h2::t) else false

let rec is_unimodal (list : int list) : bool =
	match list with
	| [] -> true
	| [x] -> true
	| h::h2::t -> if h <= h2 then is_unimodal(h2::t) else is_mon_dec(h2::t)

let rec concatEls (x : int) (pset: int list list) : int list list =
	match pset with 
	| [] -> [[x]]
	| [y] -> [x::y]
	| h::t -> (x::h) :: (concatEls x t )

let rec powerset (list: int list): int list list = 
	match list with
	| [] -> [[]]
	| h::t -> powerset(t) @ (concatEls h (powerset(t)))


let rec power (base : int) (raised : int) : int =
	if raised <= 0 then 1 else base * (power base (raised-1))

let rec rev_int (value : int) : int =
	if value = 0 then 0 else 
		let first = value mod 10 in
		let len = length (string_of_int (abs (value))) in
		(first * (power 10 (len-1))) + (rev_int (value / 10))


					
(* let rec insert_at_end (lst) (num) =
  match lst with
  | [] -> [num]
  | h :: t -> h :: (insert_at_end t num) *)

(*instead of insert_at_end just use @ *)

let rec buildSubset (k: int) (lst : 'a list)  (temp : 'a list) : 'a list * 'a list =
	match lst with
	| [] -> ([],[]) (* will never be called *)
	| h :: t -> 
		let result = 
			if (k>1) then (buildSubset (k-1) (t) (temp @ [h])) else
			((temp @ [h]) , t)
		in result

(* with corrections *)
let rec unflatten (k : int) (l : 'a list) : 'a list list option = 
	if (k <= 0) then None
	else if (List.length l <= k) then Some [l] 
	else 
		let (x, y) = (buildSubset k l []) in
		if ((List.length y) > k) then 
			let rest = match (unflatten k y) with
				| None -> [[]]
				| Some c -> c  in	
			Some ( x :: rest)
		else 
			Some ( [x] @ [y] ) 
		 




type numeral = I | V | X | L | C | D | M

type roman = numeral list

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
