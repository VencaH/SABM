open Base
open Owl_plplot

let () = Random.self_init ()
let graph_ftype = "png"

module type TestSettings = sig
    val test_name: string
    val max_temp: float
    val min_temp: float
    val step: float option
    val iterations: int option
end

module type TestModule = sig 
    val print_stats: unit -> unit
    val print_sa_avg: unit -> unit
    val print_rs_avg: unit -> unit
    val print_comparison: unit -> unit
    val print_sa: unit -> unit
    val print_rs: unit -> unit
end

module CreateTests (Inputs:TestSettings): TestModule = struct
let iteration = match Inputs.iterations with
        | Some(x) -> x
        | None -> 1000
let test_name = Inputs.test_name
let max_temp = Inputs.max_temp
let min_temp = Inputs.min_temp
let step =  match Inputs.step with
    | Some(x) -> x
    | None -> (min_temp /. max_temp) **. (1. /. (Float.of_int iteration))    

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

let schwefel (parameters : float list) : float =
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

(* simulated annealing  *)

(* Metropolis  *)

let euler : float = 2.71828182846

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
        let new_t  = t*.step_t in
         inner_sa local_size new_t min_t step_t min max cf best best_cost result in
  let start = random_params min max dimensions
  in
  let start_cost = cost_function start in
  let result = [(start_cost,start)] in
  inner_sa local_size max_t min_t step_t min max cost_function start
    start_cost result 

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
    result (fun () -> random_search 10000 5 (-500.) 500. schwefel )
let rnd_sch_10 =
    result (fun () -> random_search 10000 10 (-500.) 500. schwefel )

let sa_fdjn_5 =
    result (fun () ->simulated_annealing 10 max_temp min_temp step 5 (-5.) 5. fst_dejong) 
let sa_fdjn_10 =
    result (fun () ->simulated_annealing 10 max_temp min_temp step 10 (-5.) 5. fst_dejong) 

let sa_sdjn_5 =
    result (fun () ->simulated_annealing 10 max_temp min_temp step 5 (-5.) 5. snd_dejong) 
let sa_sdjn_10 =
    result (fun () ->simulated_annealing 10 max_temp min_temp step 10 (-5.) 5. snd_dejong) 

let sa_sch_5 =
    result (fun () ->simulated_annealing 10 max_temp min_temp step 5 (-500.) 500. schwefel) 
let sa_sch_10 =
    result (fun () ->simulated_annealing 10 max_temp min_temp step 10 (-500.) 500. schwefel) 

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

let stats data =
let len = List.nth_exn data 0 
        |> List.length
        |> Int.(+) (-1) in
let finals = List.map data ~f:(
    fun l -> List.nth_exn l len |> (fun l -> List.nth_exn l 1)
        )
    |> Array.of_list in
[Owl_stats.mean finals; Owl_stats.median finals; Owl_stats.std finals; Owl_stats.max finals; Owl_stats.min finals]

let stats_sa_fdjn_5 = stats sa_fdjn_5
let stats_sa_fdjn_10 = stats sa_fdjn_10

let stats_sa_sdjn_5 = stats sa_sdjn_5
let stats_sa_sdjn_10 = stats sa_sdjn_10

let stats_sa_sch_5 = stats sa_sch_5
let stats_sa_sch_10 = stats sa_sch_10

let stats_rnd_fdjn_5 = stats rnd_fdjn_5
let stats_rnd_fdjn_10 = stats rnd_fdjn_10

let stats_rnd_sdjn_5 = stats rnd_sdjn_5
let stats_rnd_sdjn_10 = stats rnd_sdjn_10

let stats_rnd_sch_5 = stats rnd_sch_5
let stats_rnd_sch_10 = stats rnd_sch_10

let print_line fn_name dim data = 
        Stdlib.Printf.printf
                         "|  %20s |  %2i |  %7.2f |  %7.2f |   %7.2f | %7.2f | %7.2f |" fn_name dim (List.nth_exn data 0) (List.nth_exn data 1) (List.nth_exn data 2) (List.nth_exn data 3) (List.nth_exn data 4);
        Stdlib.print_newline()

let print_stats_sa header =
        Stdlib.print_endline header;
    Stdlib.print_endline "|     cost function     | dim |   mean   |  median  | std. dev. |   max   |   min   |";
    Stdlib.print_endline "-------------------------------------------------------------------------------------";
    print_line "1st DeJong function" 5 stats_sa_fdjn_5;
    print_line "1st DeJong function" 10 stats_sa_fdjn_10;
    print_line "2nd DeJong function" 5 stats_sa_sdjn_5;
    print_line "2nd DeJong function" 10 stats_sa_sdjn_10;
    print_line "Schwefel function" 5 stats_sa_sch_5;
    print_line "Schwefel function" 10 stats_sa_sch_10

let print_stats_rnd header =
        Stdlib.print_endline header;
    Stdlib.print_endline "|     cost function     | dim |   mean   |  median  | std. dev. |   max   |   min   |";
    Stdlib.print_endline "-------------------------------------------------------------------------------------";
    print_line "1st DeJong function" 5 stats_rnd_fdjn_5;
    print_line "1st DeJong function" 10 stats_rnd_fdjn_10;
    print_line "2nd DeJong function" 5 stats_rnd_sdjn_5;
    print_line "2nd DeJong function" 10 stats_rnd_sdjn_10;
    print_line "Schwefel function" 5 stats_rnd_sch_5;
    print_line "Schwefel function" 10 stats_rnd_sch_10


let print_stats ()=
    Stdlib.Printf.sprintf "Statistic values for Simulated Annealing for %s max_temp: %.2f min_temp: %.4f step: %f"  test_name  max_temp  min_temp step
    |> print_stats_sa ;
    Stdlib.print_newline();
    print_stats_rnd "Statistic values for Random Search";
    Stdlib.print_newline()
    
 
let print_sa_avg ()=
    let no_iter data = Float.of_int (List.length data) -. 1. in
    let result data x = List.nth_exn data (Int.of_float x) in
    let filename = test_name ^ "/SA_avg.svg" in
let out = Plot.create ~m:3 ~n:2 filename in
    Plot.subplot out 0 0;
    Plot.set_title out "SA 1st DeJong function with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
    Plot.set_yrange out 0. 60.;
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
        Plot.set_yrange out 0. 5000.;
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

let print_rs_avg () =
    let result data x = List.nth_exn data (Int.of_float x) in
    let out_file = test_name ^ "/RS_avg." ^ graph_ftype in
let out = Plot.create ~m:3 ~n:2 out_file in
    Plot.subplot out 0 0;
    Plot.set_title out "Random search 1st DeJong function with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
    Plot.set_yrange out 0. 60.;
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
        Plot.set_yrange out 0. 10000.;
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

let print_comparison () =
    let no_iter data1 data2 = List.length data2
    |> Float.of_int
    |> Float.min (Float.of_int (List.length data1))
    |> Float.(+) (-1.) in
    let result data x = List.nth_exn data (Int.of_float x)  in
    let out_file = test_name ^ "/Comparison." ^ graph_ftype in
let out = Plot.create ~m:3 ~n:2 out_file in
    Plot.subplot out 0 0;
    Plot.set_title out "Comparison RS vs SA 1st DeJong function, d = 5";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
    Plot.set_yrange out 0. 60.;
    let fdjn_5_i = no_iter rnd_fdjn_5_avg sa_fdjn_5_avg in
    Plot.plot_fun ~h:out ~spec:[RGB (0,255,0)] (result rnd_fdjn_5_avg) 0. fdjn_5_i;
    Plot.plot_fun ~h:out ~spec:[RGB (0,0,255)] (result sa_fdjn_5_avg) 0. fdjn_5_i;
    Plot.legend_on out [|"Random Search";"Simulated Annealing"|];
    Plot.subplot out 0 1;
    Plot.set_title out "Comparison RS vs SA 1st DeJong function, d = 10";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 150.;
  let fdjn_10_i = no_iter rnd_fdjn_10_avg sa_fdjn_10_avg in
  Plot.plot_fun ~h:out ~spec:[RGB (0,255,0)] (result rnd_fdjn_10_avg) 0. fdjn_10_i;
  Plot.plot_fun ~h:out ~spec:[RGB (0,0,255)] (result sa_fdjn_10_avg) 0. fdjn_10_i;
    Plot.legend_on out [|"Random Search";"Simulated Annealing"|];
    Plot.subplot out 1 0;
    Plot.set_title out "Comparison RS vs SA 2nd DeJong function,  d = 5";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 2500.;
 let sdjn_5_i = no_iter rnd_sdjn_5_avg sa_sdjn_5_avg in
 Plot.plot_fun ~h:out ~spec:[RGB (0,255,0)] (result rnd_sdjn_5_avg) 0. sdjn_5_i;
 Plot.plot_fun ~h:out ~spec:[RGB (0,0,255)] (result sa_sdjn_5_avg) 0. sdjn_5_i;
    Plot.legend_on out [|"Random Search";"Simulated Annealing"|];
    Plot.subplot out 1 1;
    Plot.set_title out "Comparison RS vs SA 2nd DeJong function, d = 10";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 5000.;
let sdjn_10_i = no_iter rnd_sdjn_10_avg sa_fdjn_10_avg in
 Plot.plot_fun ~h:out ~spec:[RGB (0,255,0)] (result rnd_sdjn_10_avg) 0. sdjn_10_i;
 Plot.plot_fun ~h:out ~spec:[RGB (0,0,255)] (result sa_sdjn_10_avg) 0. sdjn_10_i;
    Plot.legend_on out [|"Random Search";"Simulated Annealing"|];
    Plot.subplot out 2 0;
    Plot.set_title out "Comparison RS vs SA Schwafel function, d = 5";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 3500.;
 let sch_5_i = no_iter rnd_sch_5_avg sa_sch_5_avg in
 Plot.plot_fun ~h:out ~spec:[RGB (0,255,0)] (result rnd_sch_5_avg) 0. sch_5_i;
 Plot.plot_fun ~h:out ~spec:[RGB (0,0,255)] (result sa_sch_5_avg) 0. sch_5_i;
    Plot.legend_on out [|"Random Search";"Simulated Annealing"|];
    Plot.subplot out 2 1;
    Plot.set_title out "Comparison RS vs SA Schwafel function, d = 10";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 5500.;
 let sch_10_i = no_iter rnd_sch_10_avg sa_sch_10_avg in
 Plot.plot_fun ~h:out ~spec:[RGB (0,255,0)] (result rnd_sch_10_avg) 0. sch_10_i;
 Plot.plot_fun ~h:out ~spec:[RGB (0,0,255)] (result sa_sch_10_avg) 0. sch_10_i;
    Plot.legend_on out [|"Random Search";"Simulated Annealing"|];
    Plot.output out;; 


(*
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
*)

let print_sa () =
  let no_iter data = Float.of_int (List.length (List.hd_exn data))  -. 1. in
  let result i data x =
      let index = Int.of_float i in
      let index_2 = Int.of_float x  in
      let res = List.nth_exn data index in
      List.nth_exn (List.nth_exn res index_2) 1 in
      let out_file = test_name ^ "/SA." ^ graph_ftype in
let out = Plot.create ~m:3 ~n:2 out_file in
    Plot.subplot out 0 0;
    Plot.set_title out "SA 1st DeJong function with 5 dimensions ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
    Plot.set_yrange out 0. 160.;
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
        Plot.set_yrange out 0. 5000.;
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

let print_rs () =
  let result i data x =
      let index = Int.of_float i in
      let index_2 = Int.of_float x in
      let res = List.nth_exn data index in
      List.nth_exn (List.nth_exn res index_2) 1 in
      let out_file = test_name ^ "/RS." ^ graph_ftype in
let out = Plot.create ~m:3 ~n:2 out_file in
    Plot.subplot out 0 0;
    Plot.set_title out "Random search 1st DeJong function with 5 dim. ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 60.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x rnd_fdjn_5) 0. 9999. ); 
    Plot.subplot out 0 1;
    Plot.set_title out "Random search 1st DeJong with 10 dim. ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 150.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x rnd_fdjn_10) 0. 9999. ) ;
    Plot.subplot out 1 0;
    Plot.set_title out "Random search 2nd DeJong with 5 dim. ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 2500.;
 List.iter (List.init 30 ~f:(fun x -> Float.of_int x)) ~f:(fun x ->Plot.plot_fun ~h:out ~spec: [RGB (Random.int 255, Random.int 255, Random.int 255)](result x rnd_sdjn_5) 0. 9999. ) ;
    Plot.subplot out 1 1;
    Plot.set_title out "Random search 2nd DeJong with 10 dim. ";
    Plot.set_xlabel out "iteration";
    Plot.set_ylabel out "CF value";
        Plot.set_yrange out 0. 10000.;
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
end

module TestSet1: TestSettings = struct
let iterations = None
let test_name = "Base"
let max_temp = 1000.
let min_temp = 0.1
let step = Some 0.98
end

module TestSet2: TestSettings = struct
let iterations = None
let test_name = "Best"
let max_temp = 10000.
let min_temp = 0.1
let step = None
end

module TestSet3: TestSettings = struct
let iterations = None
let test_name = "99_step"
let max_temp = 10000.
let min_temp = 0.1
let step = Some 0.99
end

module TestSet4: TestSettings = struct
let iterations = None
let test_name = "1000_start"
let max_temp = 1000.
let min_temp = 0.1
let step = None
end

module TestSet5: TestSettings = struct
let iterations = None
let test_name = "0.01_stop"
let max_temp = 10000.
let min_temp = 0.01
let step = None
end

module TestSet6: TestSettings = struct
let iterations = None
let test_name = "1000_0.01"
let max_temp = 1000.
let min_temp = 0.01
let step = None
end

(*
module Test1 = CreateTests (TestSet1)
let () = 
    let open Test1 in
    print_stats();
    print_rs_avg();
    print_sa_avg();
    print_sa();
    print_rs();
    print_comparison()

module Test2 = CreateTests (TestSet2)
let () = 
    let open Test2 in
    print_stats();
    print_rs_avg();
    print_sa_avg();
    print_sa();
    print_rs();
    print_comparison()

module Test3 = CreateTests (TestSet3)
let () = 
    let open Test3 in
    print_stats();
    print_rs_avg();
    print_sa_avg();
    print_sa();
    print_rs();
    print_comparison()

module Test4 = CreateTests (TestSet4)
let () = 
    let open Test4 in
    print_stats();
    print_rs_avg();
    print_sa_avg();
    print_sa();
    print_rs();
    print_comparison()

module Test5 = CreateTests (TestSet5)
let () = 
    let open Test5 in
    print_stats();
    print_rs_avg();
    print_sa_avg();
    print_sa();
    print_rs();
    print_comparison()
*)

module Test6 = CreateTests (TestSet6)
let () = 
    let open Test6 in
    print_stats();
    print_rs_avg();
    print_sa_avg();
    print_sa();
    print_rs();
    print_comparison()
