let rec is_mon_inc (list:int list):bool = 
	match list with 
	[]-> true 
	| [x] -> true 
	| [x;y] -> if x <= y then  true else false 
	| h::h2::t -> if h <= h2 then is_mon_inc(h2::t) else false 
 in 