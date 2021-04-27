type 'a result = (string, 'a) Hashtbl.t

type 'a results = (string, 'a result) Hashtbl.t

let process_results results =
  let metrics_by_test = Hashtbl.create 16 in
  Hashtbl.iter
    (fun metric_name result ->
      Hashtbl.iter
        (fun test_name ols ->
          let metrics =
            try Hashtbl.find metrics_by_test test_name
            with Not_found -> Hashtbl.create 16
          in
          Hashtbl.add metrics metric_name ols;
          Hashtbl.replace metrics_by_test test_name metrics)
        result)
    results;
  metrics_by_test


let json_of_ols ols =
  match Bechamel.Analyze.OLS.estimates ols with
  | Some [ x ] -> `Float x
  | Some estimates -> `List (List.map (fun x -> `Float x) estimates)
  | None -> `List []


let json_of_ols_results ?name (results : Bechamel.Analyze.OLS.t results) :
    Yojson.Safe.t =
  let metrics_by_test = process_results results in
  let results =
    metrics_by_test |> Hashtbl.to_seq
    |> Seq.map (fun (test_name, metrics) ->
           let metrics =
             metrics |> Hashtbl.to_seq
             |> Seq.map (fun (metric_name, ols) ->
                    (metric_name, json_of_ols ols))
             |> List.of_seq
             |> fun bindings -> `Assoc bindings
           in
           `Assoc [ ("name", `String test_name); ("metrics", metrics) ])
    |> List.of_seq
    |> fun items -> `List items
  in
  let bindings = [ ("results", results) ] in
  let bindings =
    match name with
    | Some name -> ("name", `String name) :: bindings
    | None -> bindings
  in
  `Assoc bindings
