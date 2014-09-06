let rec is_mon_inc (list:int list):bool = 
	match list with 
	[]-> true 
	| [x] -> true 
	| h::h2::t -> if h <= h2 then is_mon_inc(h2::t) else false

<<<<<<< HEAD
	(*hello






	again*)
=======
	(*try # 2*)
>>>>>>> 5d41097cf86a18af182315a2d031f97e3141b2aa
