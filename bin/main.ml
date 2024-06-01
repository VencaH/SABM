open Base
open Owl_plplot

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
      (cf : float list -> float) (best_cost : float) (best : float list) (result: (float*float list) list ) =
    match n with
    | 0 ->
        (result, best_cost, best)
    | n ->
        let params = random_params min max dim in
        let current_cost = cf params in
        (*
            Stdlib.print_float current_cost;
            Stdlib.print_newline();
*)
        if Float.( < ) current_cost best_cost then
            let result = List.append result [current_cost, params] in
          inner_rs (n - 1) dim min max cf current_cost params result
        else 
            let result = List.append result [best_cost, best] in
            inner_rs (n - 1) dim min max cf best_cost best result
  in
  let start = random_params min max dimensions in
  let start_cost = cost_function start in
  let result = [(start_cost, start)] in
  inner_rs (size - 1) dimensions min max cost_function start_cost
    start result

let print_result (fn_name : string) (res : (float * float list) list * float * float list) =
  let _,cost, params = res in
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
    (step_t : float) (min: float) (max: float) (cost_function: float list -> float) (result: (float * float list)  list) : (float * float list) list *float * float list =
  let rec inner_eval_local (size : int)(t : float)
      (min_t : float) (step_t : float) (min: float) (max: float) (cf: float list -> float) (best : float list) (best_cost : float) (result: (float * float list)  list) :
      (float * float list)  list * float * float list =
    match size with
    | 0 ->
        (result,best_cost, best)
    | x ->
        let next_input = local_next best min max in
        let next_cost = cf next_input in
        let best_cost, best =
          metropolis t best best_cost next_input next_cost
        in
        let result = List.append result [best_cost,best] in
        inner_eval_local (x - 1) t min_t step_t min max cf best best_cost result
  in
  inner_eval_local size t min_t step_t min max cost_function start (cost_function start) result

let simulated_annealing (local_size : int) (max_t : float)
    (min_t : float) (step_t : float) (dimensions: int) (min: float) (max: float) (cost_function: float list -> float) =
  let rec inner_sa (local_size : int) (t : float)
      (min_t : float) (step_t : float) (min: float) (max:float) (cf: float list -> float) (best : float list)
      (best_cost : float) (result: (float * float list) list) :(float * float list) list * float * float list =
    match t with
    | t when Float.(<) t min_t ->
        (result, best_cost, best)
    | t ->
        let result,best_cost, best =
          eval_local local_size best t min_t step_t min max cf result
        in
        (*let result = List.append result [(best_cost, best)] in *)
        let new_t  = t*.step_t in
         inner_sa local_size new_t min_t step_t min max cf best best_cost result in
  let start = random_params min max dimensions
  in
  let start_cost = cost_function start in
  let result = [(start_cost,start)] in
  inner_sa local_size max_t min_t step_t min max cost_function start
    start_cost result 
(*
let () =
  simulated_annealing 10 1000. 0.1 0.88 5 (-5.) 5. fst_dejong
  |> print_result "SA 1st DeJong function";
  simulated_annealing 10 1000. 0.1 0.88 5 (-5.) 5. snd_dejong
  |> print_result "SA 2nd DeJong function";
  simulated_annealing 10 1000. 1. 0.999079 10  (-500.) 500. schweffel
  |> print_result "SA Schwefel function"
*)

let result fn =
    let accumulator i acc _ =
        let (result, _,_) = fn() in
        let row =List.map result ~f:(fun res -> let (cost, inp) = res in List.append [cost] inp  |> List.append [Float.of_int i]) in
        List.append acc [row] in
    List.foldi (List.init 30 ~f:(fun _ -> 0)) ~init: [] ~f:accumulator

let avg_result (result: float list list list) =
    let reps = Float.of_int (List.length result) in
    let len = List.length (List.hd_exn result) in
    let accumulator acc res =
        List.map2_exn acc res ~f:(fun a b -> a +. (List.nth_exn b 1)) in
    let sum =List.fold result ~init:(List.init len ~f:(fun _ -> 0.)) ~f: accumulator in
    List.map sum ~f:(fun x -> Float.(/) x reps)

let rnd_fdjn_5 =
    result (fun () -> random_search 10000 5 (-5.) 5. fst_dejong )
let rnd_fdjn_10 =
    result (fun () -> random_search 10000 10 (-5.) 5. fst_dejong )

let rnd_sdjn_5 =
    result (fun () -> random_search 10000 5 (-5.) 5. snd_dejong )
let rnd_sdjn_10 =
    result (fun () -> random_search 10000 10 (-5.) 5. snd_dejong )

let rnd_sch_5 =
    result (fun () -> random_search 10000 5 (-500.) 500. schweffel )
let rnd_sch_10 =
    result (fun () -> random_search 10000 10 (-500.) 500. schweffel )

let sa_fdjn_5 =
    result (fun () ->simulated_annealing 10 1000. 0.1 0.990832 5 (-5.) 5. fst_dejong) 
let sa_fdjn_10 =
    result (fun () ->simulated_annealing 10 1000. 0.1 0.990832 10 (-5.) 5. fst_dejong) 


let sa_sdjn_5 =
    result (fun () ->simulated_annealing 10 1000. 0.1 0.990832 5 (-5.) 5. snd_dejong) 
let sa_sdjn_10 =
    result (fun () ->simulated_annealing 10 1000. 0.1 0.990832 10 (-5.) 5. snd_dejong) 

let sa_sch_5 =
    result (fun () ->simulated_annealing 10 1000. 0.1  0.990832 5 (-500.) 500. schweffel) 
let sa_sch_10 =
    result (fun () ->simulated_annealing 10 1000. 0.1  0.990832 10 (-500.) 500. schweffel) 

let sa_fdjn_5_avg = 
    avg_result sa_fdjn_5
let sa_fdjn_10_avg = 
    avg_result sa_fdjn_10

let sa_sdjn_5_avg = 
    avg_result sa_sdjn_5
let sa_sdjn_10_avg = 
    avg_result sa_sdjn_10

let sa_sch_5_avg = 
    avg_result sa_sch_5
let sa_sch_10_avg = 
    avg_result sa_sch_10

let rnd_fdjn_5_avg = 
    avg_result rnd_fdjn_5
let rnd_fdjn_10_avg = 
    avg_result rnd_fdjn_10

let rnd_sdjn_5_avg = 
    avg_result rnd_sdjn_5
let rnd_sdjn_10_avg = 
    avg_result rnd_sdjn_10

let rnd_sch_5_avg = 
    avg_result rnd_sch_5
let rnd_sch_10_avg = 
    avg_result rnd_sch_10




let () =
    let no_iter data = Float.of_int (List.length data) -. 1. in
    let result data x = List.nth_exn data (Int.of_float x) in
let out = Plot.create ~m:3 ~n:2 "SA_avg.png" in
    Plot.subplot out 0 0;
    Plot.set_title out "SA 1st DeJong function with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
    Plot.set_yrange out 0. 125.;
    Plot.plot_fun ~h:out (result sa_fdjn_5_avg) 0. (no_iter sa_fdjn_5_avg);
    Plot.subplot out 0 1;
    Plot.set_title out "SA 1st DeJong with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 150.;
  Plot.plot_fun ~h:out (result sa_fdjn_10_avg) 0. (no_iter sa_fdjn_10_avg);
    Plot.subplot out 1 0;
    Plot.set_title out "SA 2nd DeJong with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 2500.;
 Plot.plot_fun ~h:out (result sa_sdjn_5_avg) 0. (no_iter sa_sdjn_5_avg);
    Plot.subplot out 1 1;
    Plot.set_title out "SA 2nd DeJong with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 2500.;
 Plot.plot_fun ~h:out (result sa_sdjn_10_avg) 0. (no_iter sa_sdjn_10_avg);
    Plot.subplot out 2 0;
    Plot.set_title out "SA Schwafel with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 3500.;
 Plot.plot_fun ~h:out (result sa_sch_5_avg) 0. (no_iter sa_sch_5_avg);
    Plot.subplot out 2 1;
    Plot.set_title out "SA Schwafel with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 5500.;
 Plot.plot_fun ~h:out (result sa_sch_10_avg) 0. (no_iter sa_sch_10_avg);
    Plot.output out;; 

let () =
    let result data x = List.nth_exn data (Int.of_float x) in
let out = Plot.create ~m:3 ~n:2 "RS_avg.png" in
    Plot.subplot out 0 0;
    Plot.set_title out "Random search 1st DeJong function with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
    Plot.set_yrange out 0. 125.;
    Plot.plot_fun ~h:out (result rnd_fdjn_5_avg) 0. 9999.;
    Plot.subplot out 0 1;
    Plot.set_title out "Random search 1st DeJong with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 150.;
  Plot.plot_fun ~h:out (result rnd_fdjn_10_avg) 0. 9999.;
    Plot.subplot out 1 0;
    Plot.set_title out "Random search 2nd DeJong with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 2500.;
 Plot.plot_fun ~h:out (result rnd_sdjn_5_avg) 0. 9999.;
    Plot.subplot out 1 1;
    Plot.set_title out "Random search 2nd DeJong with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 2500.;
 Plot.plot_fun ~h:out (result rnd_sdjn_10_avg) 0. 9999.;
    Plot.subplot out 2 0;
    Plot.set_title out "Random search Schwafel with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 3500.;
 Plot.plot_fun ~h:out (result rnd_sch_5_avg) 0. 9999.;
    Plot.subplot out 2 1;
    Plot.set_title out "Random search Schwafel with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 5500.;
 Plot.plot_fun ~h:out (result rnd_sch_10_avg) 0. 9999.;
    Plot.output out;; 

let () =
    let result data x = List.nth_exn data (Int.of_float x)  in
let out = Plot.create ~m:3 ~n:2 "Comparison.png" in
    Plot.subplot out 0 0;
    Plot.set_title out "Random search 1st DeJong function with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
    Plot.set_yrange out 0. 125.;
    Plot.plot_fun ~h:out (result rnd_fdjn_5_avg) 0. 5500.;
    Plot.plot_fun ~h:out (result sa_fdjn_5_avg) 0. 5500.;
    Plot.subplot out 0 1;
    Plot.set_title out "Random search 1st DeJong with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 150.;
  Plot.plot_fun ~h:out (result rnd_fdjn_10_avg) 0. 5500.;
  Plot.plot_fun ~h:out (result sa_fdjn_10_avg) 0. 5500.;
    Plot.subplot out 1 0;
    Plot.set_title out "Random search 2nd DeJong with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 2500.;
 Plot.plot_fun ~h:out (result rnd_sdjn_5_avg) 0. 5500.;
 Plot.plot_fun ~h:out (result sa_sdjn_5_avg) 0. 5500.;
    Plot.subplot out 1 1;
    Plot.set_title out "Random search 2nd DeJong with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 2500.;
 Plot.plot_fun ~h:out (result rnd_sdjn_10_avg) 0. 5500.;
 Plot.plot_fun ~h:out (result sa_sdjn_10_avg) 0. 5500.;
    Plot.subplot out 2 0;
    Plot.set_title out "Random search Schwafel with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 3500.;
 Plot.plot_fun ~h:out (result rnd_sch_5_avg) 0. 5500.;
 Plot.plot_fun ~h:out (result sa_sch_5_avg) 0. 5500.;
    Plot.subplot out 2 1;
    Plot.set_title out "Random search Schwafel with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 5500.;
 Plot.plot_fun ~h:out (result rnd_sch_10_avg) 0. 5500.;
 Plot.plot_fun ~h:out (result sa_sch_10_avg) 0. 5500.;
    Plot.output out;; 



let print_result (res: float list list list) = 
    let print_line o (l: float list) = 
        List.iter l ~f:(fun x -> Stdlib.Printf.fprintf o "%f;" x) in
    let out_file = Stdlib.open_out "test.txt" in
    List.iter res ~f:(fun res -> List.iter res ~f:(fun l -> print_line out_file l; Stdlib.Printf.fprintf out_file "\n"));
    Stdlib.close_out out_file
let () =
    print_result rnd_sdjn_10

let print_avg (res: float list) = 
    let out_file = Stdlib.open_out "avg.txt" in
    List.iter res ~f:(fun res -> Stdlib.Printf.fprintf out_file "%f;\n" res)
let () =
    print_avg sa_fdjn_5_avg


let () =
  let no_iter data = Float.of_int (List.length (List.hd_exn data))  -. 1. in
  let result i data x =
      let index = Int.of_float i in
      let index_2 = Int.of_float x  in
      let res = List.nth_exn data index in
      List.nth_exn (List.nth_exn res index_2) 1 in
let out = Plot.create ~m:3 ~n:2 "SA.png" in
    Plot.subplot out 0 0;
    Plot.set_title out "SA 1st DeJong function with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
    Plot.set_yrange out 0. 125.;
    List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x sa_fdjn_5) 0. (no_iter sa_fdjn_5) ); 
    Plot.subplot out 0 1;
    Plot.set_title out "SA 1st DeJong with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 150.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x sa_fdjn_10) 0. (no_iter sa_fdjn_10) ) ;
    Plot.subplot out 1 0;
    Plot.set_title out "SA 2nd DeJong with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 2500.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x sa_sdjn_5) 0. (no_iter sa_sdjn_5) ) ;
    Plot.subplot out 1 1;
    Plot.set_title out "SA 2nd DeJong with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 2500.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x sa_sdjn_10) 0. (no_iter sa_sdjn_10) ) ;
    Plot.subplot out 2 0;
    Plot.set_title out "SA Schwafel with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 3500.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x sa_sch_5) 0. (no_iter sa_sch_5) ) ;
    Plot.subplot out 2 1;
    Plot.set_title out "SA Schwafel with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 5500.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x sa_sch_10) 0. (no_iter sa_sch_10) ) ;
    Plot.output out;; 

let () =
  let result i data x =
      let index = Int.of_float i in
      let index_2 = Int.of_float x in
      let res = List.nth_exn data index in
      List.nth_exn (List.nth_exn res index_2) 1 in
let out = Plot.create ~m:3 ~n:2 "RS.png" in
    Plot.subplot out 0 0;
    Plot.set_title out "Random search 1st DeJong function with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 125.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x rnd_fdjn_5) 0. 9999. ); 
    Plot.subplot out 0 1;
    Plot.set_title out "Random search 1st DeJong with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 150.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x rnd_fdjn_10) 0. 9999. ) ;
    Plot.subplot out 1 0;
    Plot.set_title out "Random search 2nd DeJong with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 2500.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x rnd_sdjn_5) 0. 9999. ) ;
    Plot.subplot out 1 1;
    Plot.set_title out "Random search 2nd DeJong with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 2500.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x rnd_sdjn_10) 0. 9999. ) ;
    Plot.subplot out 2 0;
    Plot.set_title out "Random search Schwafel with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 3500.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x rnd_sch_5) 0. 9999. ) ;
    Plot.subplot out 2 1;
    Plot.set_title out "Random search Schwafel with 10 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 5500.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x rnd_sch_10) 0. 9999. ) ;
    Plot.output out;;


