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


(* PLACE HOLDER FOR POWERSET FUNCTION*)



let rec power (base : int) (raised : int) : int =
	if raised <= 0 then 1 else base * (power base (raised-1))

let rec rev_int (value : int) : int =
	if value = 0 then 0 else 
		let first = value mod 10 in
		let len = length (string_of_int (abs (value))) in
		(first * (power 10 (len-1))) + (rev_int (value / 10))



	
