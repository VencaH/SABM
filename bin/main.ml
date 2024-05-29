open Base

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
      acc +. (x *. Stdlib.sin (Stdlib.sqrt (Stdlib.abs_float x))) )
  |> Float.( - ) (418.9829 *. d)

let random_search (size : int) (dimensions : int) (min : float) (max : float)
    (cost_function : float list -> float) =
  let random_params min max dim =
    List.init dim ~f:(fun _ -> Random.float (max -. min) +. min)
  in
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
