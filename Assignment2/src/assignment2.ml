open List

let rec cond_dup l f =
  match l with
  | [] -> []
  | (h :: t) -> if (f h) then (h :: h :: cond_dup t f) else (h :: cond_dup t f)

let rec n_times (f, n, v) =
  if n <= 0 then
    v
  else if n = 1 then
    (f v)
  else
    let v2 = (f v) in
    n_times (f, n-1, v2)

let rec zipwith f l1 l2 =
  match l1 with
  | [] -> []
  | (h1 :: t1) -> (match l2 with
    | [] -> []
    | (h2 :: t2) -> f h1 h2 :: zipwith f t1 t2)
    


let first l =
  match l with
  | [] -> []
  | (h :: t) -> h

(* Return true if target is in the given bucket. *)
let rec is_in_bucket target bucket p =
  match bucket with
  | [] -> false
  | (h :: t) -> if (p target h) then true else is_in_bucket target t p

(*let rec buckets_add p l =
  match l with
  | [] -> []
  | (h :: t) -> [h] :: buckets_add p t

let rec merge_bucket target buckets p =
  match buckets with
  | [] -> buckets
  | (h :: t) -> if p (first target) (first h) then (target @ h) :: merge_bucket (target @ h) t p else (target) :: merge_bucket target t p

let rec buckets_sort sorted unsorted p =
  match unsorted with
  | [] -> sorted
  | (h1 :: t1) -> (let newly_sorted = (merge_bucket h1 t1 p) in match newly_sorted with
    | [] -> newly_sorted
    | (h2 :: t2) -> buckets_sort newly_sorted t2 p) *)


(* Create a bucket with first element
Add all equivalent elements in list to that bucket
Remove all equivalent elements from list
Repeat until list is empty -> return list of buckets *)

(* Add all equivalent elements to target in l to a bucket, assuming target is first element in bucket. *)
let rec bucket_add target bucket l p =
  match l with
  | [] -> bucket
  | (h :: t) -> if (p target h) then bucket_add target (bucket @ [h]) t p else bucket_add target (bucket) t p

(* Remove all elements equivalent to target from l. *)
let rec remove_equiv_elements target l newlist p =
  match l with
  | [] -> newlist
  | (h :: t) ->
    if (p target h) then
      remove_equiv_elements target t (newlist) p
    else
      remove_equiv_elements target t (newlist @ [h]) p

let rec buckets_create buckets l p =
  match l with
  | [] -> buckets
  | (h1 :: t1) ->
    let new_bucket = bucket_add h1 [h1] t1 p in
    let removed_duplicates = remove_equiv_elements h1 l [] p in
    buckets_create (buckets @ [new_bucket]) removed_duplicates p

(*
  (match buckets with
    | [] -> let new_bucket = bucket_add h1 ([h1] :: buckets) t1 p
    | (h2 :: t2) ->
      if (is_in_bucket h1 h2 p) then
        buckets_create ((bucket_add h1 h2 t1 p) :: t2) t1 p
      else
        buckets_create  ) *)



(*let rec buckets_sort p sorted_buckets l =
  match l with
  | [] -> []
  | (h1 :: t1) -> (match t1 with
    | (h2 :: t2) -> if (p h1 h2) then h1 @ h2 :: buckets_sort p t2 l else buckets_sort)  *)

let buckets p l =
  buckets_create [] l p
  (*let single_buckets = buckets_add p l in
  buckets_sort [[]] single_buckets p *)

  (*buckets_add [[]] l p*)

  



let rec fib n prev cur =
  if n = 0 then 0
  else if n = 1 then cur
  else fib (n-1) (cur) (prev+cur)

let fib_tailrec n =
  fib n 0 1

let assoc_list lst =
  let count a e = a + e in
  let count_elements target = List.fold_left count 0 (List.map (fun e -> if e = target then 1 else 0) lst) in

  let create_tuple a e = (e, count_elements e) :: a in
  let tuples = List.fold_left create_tuple [] lst in

  let is_duplicate t a = List.fold_left (fun a2 t2 -> a2 || t2 = t) false a in
  let remove_duplicates = List.fold_left (fun a t -> if (is_duplicate t a) then a else (t :: a)) [] tuples in
  remove_duplicates

let ap fs args =
  let y = List.map (fun x -> List.map x args) fs in
  let flatten h t = h @ t in
  List.fold_left flatten [] y


(********)
(* Done *)
(********)


let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in
  (*let print_int_list_list = List.iter (List.iter (Printf.printf "%d ")) in
  print_int_list_list (buckets (=) [1;2;3;4]) *)
  
  (* Testcases for cond_dup *)
  let _ =
    try
      assert (cond_dup [3;4;5] (fun x -> x mod 2 = 1) = [3;3;4;5;5])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for n_times *)
  let _ =
    try
      assert (n_times((fun x-> x+1), 50, 0) = 50)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for zipwith *)
  let _ =
    try
      assert ([5;7] = (zipwith (+) [1;2;3] [4;5]))
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for buckets *)
  let _ =
    try
      assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]);
      assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]);
      assert (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fib_tailrec *)
  let _ =
    try
      assert (fib_tailrec 50 = 12586269025)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for assoc_list *)
  let _ =
    let y = ["a";"a";"b";"a"] in
    let z = [1;7;7;1;5;2;7;7] in
    let cmp x y = if x < y then (-1) else if x = y then 0 else 1 in
    try
      assert ([("a",3);("b",1)] = List.sort cmp (assoc_list y));
      assert ([(1,2);(2,1);(5,1);(7,4)] = List.sort cmp (assoc_list z));
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for ap *)
  let _ =
    let x = [5;6;7;3] in
    let b = [3] in
    let fs1 = [((+) 2) ; (( * ) 7)] in
    try
      assert  ([7;8;9;5;35;42;49;21] = ap fs1 x);
      assert  ([5;21] = ap fs1 b);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  Printf.printf ("%d out of 7 programming questions are incorrect.\n") (!error_count)

let _ = main()
