open Bechamel
open Toolkit

let make_all n =
  let open Streaming in
  Staged.stage @@ fun () ->
  Stream.(0 -- n)
  |> Stream.map (fun x -> x + 1)
  |> Stream.filter (fun x -> x mod 3 = 0)
  |> Stream.take (n / 2)
  |> Stream.flat_map (fun x -> Stream.(x -- (x + 30)))
  |> Stream.fold ( + ) 0


let test =
  Test.make_indexed ~name:"all" ~fmt:"%s %d"
    ~args:[ 10; 100; 1_000; 10_000; 100_000; 1_000_000 ]
    make_all


let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 0.5) ~kde:(Some 1000) ()
  in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)


let output_current_bench () =
  let results, _ = benchmark () in
  let json : Yojson.Safe.t =
    Current_bench_bechamel.json_of_ols_results results
  in
  Yojson.Safe.pretty_to_channel stdout json;
  print_newline ()


let output_notty () =
  List.iter
    (fun v -> Bechamel_notty.Unit.add v (Measure.unit v))
    Instance.[ minor_allocated; major_allocated; monotonic_clock ];

  let img (window, results) =
    Bechamel_notty.Multiple.image_of_ols_results ~rect:window
      ~predictor:Measure.run results
  in

  let open Notty_unix in
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  let results, _ = benchmark () in
  img (window, results) |> eol |> output_image


let main format =
  match format with
  | `Json -> output_current_bench ()
  | `Table -> output_notty ()


let () =
  let open Cmdliner in
  let format =
    let doc = "Benchmark output format." in
    Arg.(
      required
      & opt (some (enum [ ("json", `Json); ("table", `Table) ])) None
      & info [ "format" ] ~doc)
  in

  let info =
    let doc = "Run the main streaming benchmark." in
    Term.info "streaming-bench-main" ~doc
  in
  let term = Term.(const main $ format) in
  Term.exit @@ Term.eval (term, info)
