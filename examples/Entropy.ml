
let answer () =
  let x = "84726982693273833265833289698432737883857070736773697" ^
          "8843268658465327079823265327769657873787170857632657883876982
  What if there was no space and time?" in
  let open Format in let (%^) = printf in
  let open Streaming in let open Stream in
  let (%)=fun f g x->f (g x) and
  (|/-)=(%^)"\027[1m\027[41m";"\\|/-"in let (//) x = (|/-)
    |> of_string
    |> interpose '\b' |> append '\b'
    |> cycle ~times:(Random.int 10)
    |> append x
    |> each (fun c -> "%c%!" %^ (Unix.sleepf 0.003; c)) in
  from @@ (let pull i =
      if i >= 114 then None
      else Some (String.sub x i 2, i + 2)
    in Source.make ~init:(fun () -> 0) ~pull ())
  |> via (Flow.map (char_of_int%int_of_string))
  |> into (Sink.each (//)); (%^) "\027[0m"


;; answer ()
