open String

(* Precondition: takes an integer list
 * Postcontition: returns a boolean that is true if the list is 
 * monotonically increasing and false otherwise  *)
let rec is_mon_inc (l: int list) : bool = 
    match l with 
    | []-> true 
    | [x] -> true 
    | h::h2::t -> if h <= h2 then is_mon_inc(h2::t) else false

(* Helper function for is_unimodal 
 * Precondition: takes an integer list 
 * Postcondition: returns a boolean that is true if the list is
 * monotonically decreasing, false otherwise *)
(* CHANGED HERE *)

(* Precondition: takes an integer list 
 * Postcondition: returns a boolean that is true if the list is
 * unimodal, false otherwise *)
let rec is_unimodal (l : int list) : bool =
    let rec is_mon_dec (l : int list) : bool =
        match l with
        | [] -> true
        | [x] -> true
        | h::h2::t -> (if h>=h2 then is_mon_dec(h2::t) else false) in
    match l with
    | [] -> true
    | [x] -> true
    | h::h2::t -> if h <= h2 then is_unimodal(h2::t) else is_mon_dec(h2::t)

(* Helper function for powerset 
 * Precondition: takes an integer x and a powerset, which is a list of int 
 * list 
 * Postcondition: concatenates x onto each element in the powerset *)


(* Precondition: takes an int list 
 * Postcondition: returns the powerset of the set of integers in the list *)
let rec powerset (l: int list): int list list = 
    (* powerset helper function *)
    let rec concatEls (x : int) (pset: int list list) : int list list =
        match pset with 
        | [] -> [[x]]
        | [y] -> [x::y]
        | h::t -> ((x::h) :: (concatEls x t )) in
    match l with
    | [] -> [[]]
    | h::t -> powerset(t) @ (concatEls h (powerset(t)))

(* Helper function for rev_int 
 * Precondition: takes two integer values 
 * Postcondition: returns an integer value equivalent to
 * the first int raised to the power of the second int *)


(* Precondition: takes an integer value <= than max_int whose reversed
 * arrangement is also <= than max_int
 * Postcondition: returns the integer with the digits of the
 * input reversed *)
let rec rev_int (value : int) : int =
    (* rev_int helper function *)
    let rec power (base : int) (raised : int) : int =
        if raised <= 0 then 
            1 
        else 
            (base * (power base (raised-1))) in
    if value = 0 then 
        0 
    else 
        let first = value mod 10 in
        let len = length (string_of_int (abs (value))) in
        (first * (power 10 (len-1))) + (rev_int (value / 10))
        
(* Helper function for unflatten
 * Precondition: takes an int k, an 'a list to generate a sublist off of, and 
 * an 'a list temp which serves as an accumulator list
 * Postcondition: returns an 'a list * 'a list tuple where the first
 * value represents the first k elements of lst and the second element 
 * represents the rest of the original lst *)


(* Precondition: takes an 'a list and an integer k
 * Postcondition: returns an 'a list list option that contains the original
 * list broken up into subsets of size k. If k does not divide inevenly, 
 * the last 'a list will contain fewer than k elements *)
let rec unflatten (k : int) (l : 'a list) : 'a list list option = 
    (* unflatten helper function *)
    let rec build_subset (k: int)(lst : 'a list)(temp : 'a list):'a list*'a list=
        match lst with
        | [] -> ([],[]) 
        | h :: t -> (
            if (k>1) then 
                (build_subset (k-1) (t) (temp @ [h])) 
            else
                ((temp @ [h]) , t)) in
    if (k <= 0) then None
    else if (List.length l <= k) then Some [l] 
    else 
        let (x, y) = (build_subset k l []) in
        if ((List.length y) > k) then 
            let rest = 
                match (unflatten k y) with
                | None -> [[]]
                | Some c -> c  in   
            Some ( x :: rest)
        else 
            Some ( [x] @ [y] ) 
         
type numeral = I | V | X | L | C | D | M

type roman = numeral list

(* Precondition: takes a value of type Roman that must be valid as defined 
 * by ps1 handout
 * Postcondition: returns the integer equivalent of the roman type *)
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
    | h1 :: h2 :: tl -> (
        if (int_of_numeral h1)>=(int_of_numeral h2) then
            (int_of_numeral h1) + (int_of_roman (h2::tl)) 
        else
            ~-(int_of_numeral h1) + (int_of_roman (h2::tl)))




