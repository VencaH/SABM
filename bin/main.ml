open Base
open Owl

let () = Random.self_init ()

let fst_dejong (parameters : float list) : float =
  List.fold parameters ~init:0. ~f:(fun acc x -> acc +. (x **. 2.))

let snd_dejong (parameters : float list) : float =
  let rec inner_snd_dejong (n : int) (p : float list) (acc : float) : float =
    match n with
    | x when x = List.length p - 2 ->
        acc
    | x ->
        let fst = List.nth_exn p x in
        let snd = List.nth_exn p (x + 1) in
        let new_acc =
          acc +. (100. *. ((snd -. (fst **. 2.)) **. 2.)) +. ((1. -. fst) **. 2.)
        in
        inner_snd_dejong (n + 1) p new_acc
  in
  inner_snd_dejong 0 parameters 0.

let schweffel (parameters : float list) : float =
  let d = Float.of_int (List.length parameters) in
  List.fold parameters ~init:0. ~f:(fun acc x ->
      acc +. (x *. Float.sin (Float.sqrt (Float.abs x))) )
  |> Float.( - ) (418.9829 *. d)


  let random_params min max dim =
    List.init dim ~f:(fun _ -> Random.float (max -. min) +. min)

let random_search (size : int) (dimensions : int) (min : float) (max : float)
    (cost_function : float list -> float) =
  let rec inner_rs (n : int) (dim : int) (min : float) (max : float)
      (cf : float list -> float) (best_cost : float) (best : float list) =
    match n with
    | 0 ->
        (best_cost, best)
    | n ->
        let params = random_params min max dim in
        let current_cost = cf params in
        (*
            Stdlib.print_float current_cost;
            Stdlib.print_newline();
*)
        if Float.( < ) current_cost best_cost then
          inner_rs (n - 1) dim min max cf current_cost params
        else inner_rs (n - 1) dim min max cf best_cost best
  in
  let start = random_params min max dimensions in
  inner_rs (size - 1) dimensions min max cost_function (cost_function start)
    start

let print_result (fn_name : string) (res : float * float list) =
  let cost, params = res in
  Stdlib.print_string fn_name ;
  Stdlib.print_endline " random search result: " ;
  Stdlib.print_string "Minimum: " ;
  Stdlib.print_float cost ;
  Stdlib.print_newline () ;
  Stdlib.print_string "Values: " ;
  List.iter params ~f:(fun p -> Stdlib.print_float p ; Stdlib.print_string " ") ;
  Stdlib.print_newline ()

let () =
  let rs = random_search 10000 5 in
  rs (-5.) 5. fst_dejong |> print_result "1st Dejong function" ;
  rs (-5.) 5. snd_dejong |> print_result "2nd Dejong function" ;
  rs (-500.) 500. schweffel |> print_result "Schweffel function"

(* simulated annealing  *)

(* Metropolis  *)

let euler : float = 2.71828

let metropolis (t : float) (best : float list)
    (best_cost : float) (current : float list) (current_cost : float) :
    float * float list =
  let diff = current_cost -. best_cost in
  if Float.( < ) diff 0. then
    ( current_cost, current)
  else
    let prob = 1. /. Float.( ** ) euler (diff /. t) in
    if Float.( > ) prob (Random.float 1.) then
      (current_cost, current)
    else ( best_cost, best)

(* step in local space *)

(* generate new value from gaussian dist  *)
let pi = 4. *. Float.atan 1.
let rec random_local (dim_min:float) (dim_max:float) (current: float) =
let box_muller() =
let x = Random.float 1. in
let y = Random.float 2. in
Float.sqrt((-2.) *. (Float.log x)) *. Float.cos (2. *. y *.pi) in
let rec increment() =
let inc = box_muller() *. ((dim_max -. dim_min)/.60.) in
if (Float.(<) inc  (dim_min /. 10.)) || (Float.(>) inc  (dim_max /. 10.)) then increment()
else inc in
let new_value = current +. increment() in
if (Float.(<) new_value  dim_min) || (Float.(>) new_value dim_max) then random_local dim_min dim_max current
else new_value;;
  

let local_next (previous: float list) (min: float) (max: float): float list =
  let generate = random_local min max in
  List.map previous ~f:generate 
  

let eval_local (size : int) (start : float list) (t : float) (min_t : float)
    (step_t : float) (min: float) (max: float) (cost_function: float list -> float) : float * float * float list =
  let rec inner_eval_local (size : int) (current : float list) (t : float)
      (min_t : float) (step_t : float) (min: float) (max: float) (cf: float list -> float) (best : float list) (best_cost : float) :
      float * float * float list =
    match size with
    | 0 ->
        (t, best_cost, best)
    | x ->
        let current_cost = cf current in
        let best_cost, best =
          metropolis t best best_cost current current_cost
        in
        inner_eval_local (x - 1) (local_next current min max) t min_t step_t min max cf best best_cost
  in
  inner_eval_local size start t min_t step_t min max cost_function start (999999.)

let simulated_annealing (size : int) (local_size : int) (max_t : float)
    (min_t : float) (step_t : float) (dimensions: int) (min: float) (max: float) (cost_function: float list -> float) =
  let rec inner_sa (size : int) (local_size : int) (iter : int) (t : float)
      (min_t : float) (step_t : float) (min: float) (max:float) (cf: float list -> float) (current : float list) (best : float list)
      (best_cost : float) : float * float list =
    match size with
    | 0 ->
        (best_cost, best)
    | x ->
        let local_size = Int.min size local_size in
        let t, local_best_cost, current =
          eval_local local_size current t min_t step_t min max cf
        in
        let iter = iter + local_size in
        let new_t  = Float.max (t*.step_t) min_t in
        let local_sa =
          inner_sa (x - local_size) local_size iter new_t min_t step_t min max cf current
        in
        if Float.(<) local_best_cost best_cost then local_sa current local_best_cost
        else local_sa best best_cost
  in
  let current = random_params min max dimensions
  in
  inner_sa size local_size 1 max_t min_t step_t min max cost_function current current
    (cost_function current)

let () =
  simulated_annealing 10000 10 1000. 0.1 0.1 5 (-5.) 5. fst_dejong
  |> print_result "SA 1st DeJong function";
  simulated_annealing 10000 10 1000. 0.1 0.1 5 (-5.) 5. snd_dejong
  |> print_result "SA 2nd DeJong function";
  simulated_annealing 10000 10 1000. 0.01 0.01 20  (-500.) 500. schweffel
  |> print_result "SA Schwefel function";

let () =
  

(*
let () =
  let fdj x = fun _ ->  simulated_annealing 10000 10 1000. 0.1 0.1 5 (-5.) 5. fst_dejong in
    let out = Plot.create "plot_test.png" in
    Plot.set_title h "test test";
    Plot.set_xlabel "iteration";
    Plot.set_ylabel "CF value";
    Plot.plot_fun ~h f 1. 30.;
    Plot.output h 
*)
