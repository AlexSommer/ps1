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


					
let rec insert_at_end (lst) (num) =
  match lst with
  | [] -> [num]
  | h :: t -> h :: (insert_at_end t num)

let rec buildSubset (k: int) (lst : 'a list)  (temp : 'a list) : 'a list * 'a list =
	match lst with
	| [] -> ([],[]) (*is this correct??*)
	| h :: t -> 
		let result = 
			if (k>1) then (buildSubset (k-1) (t) (insert_at_end temp h)) else
			((insert_at_end temp h) , t)
		in result


let rec unflatten (k : int) (l : 'a list) : 'a list list option = 
	if (k = 0 ) then None else
	match l with 
	| [] -> Some []
	| [x] -> Some [[x]]
	| _ -> 
			if (List.length l <= k) then Some [l]
		else 
			let (x, y) = buildSubset k l [] in
			if (List.length y) > k then 
				let rest = 
					match unflatten k y with
					None -> []
					| Some c -> c  in
				Some ( x :: rest)
			else Some ( [x] @ [y] ) 
		 
	
