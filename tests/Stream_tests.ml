open Streaming
module T = Nanotest
module S = Stream


let () =
  let verbose = true in

  let t = T.test T.int ~verbose in
  T.group "Stream.empty" [
    t "empty length" ~actual:(S.len S.empty) ~expected:0;
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Stream.single" [
    t "same value" ~actual:(S.to_list (S.single 42)) ~expected:[42];
  ];
  T.group "Stream.double" [
    t "same values" ~actual:(S.to_list (S.double 1 2)) ~expected:[1; 2];
  ];
  T.group "Stream.triple" [
    t "same values" ~actual:(S.to_list (S.triple 1 2 3)) ~expected:[1; 2; 3];
  ];
  T.group "Stream.of_list" [
    t "same values" ~actual:(S.to_list (S.of_list [1; 2; 3])) ~expected:[1; 2; 3];
  ];
  T.group "Stream.count" [
    t "empty" ~actual:S.(to_list (take 0 (count 0))) ~expected:[];
    t "take 4" ~actual:S.(to_list (take 4 (count 5))) ~expected:[5; 6; 7; 8];
  ];
  T.group "Stream.range" [
    t "empty" ~actual:S.(to_list (range 0 0)) ~expected:[];
    t "single" ~actual:S.(to_list (range 0 1)) ~expected:[0];
    t "negative" ~actual:S.(to_list (range (-3) 1)) ~expected:[-3; -2; -1; 0];
    t "step" ~actual:S.(to_list (range 0 50 ~by:10)) ~expected:[0; 10; 20; 30; 40];
    T.raises "invalid" (fun () -> S.range 10 0)
      ~exn:(Invalid_argument "Streaming.Stream.range: invalid range");
  ];
  T.group "Stream.iota" [
    t "empty" ~actual:S.(to_list (iota 0)) ~expected:[];
    t "single" ~actual:S.(to_list (iota 1)) ~expected:[0];
    T.raises "negative" (fun () -> S.iota (-3))
      ~exn:(Invalid_argument "Streaming.Stream.range: invalid range");
  ];
  let t = T.test T.(list int) ~verbose in
  T.group "Stream.generate" [
    t "empty" ~actual:S.(to_list (generate ~len:0 (fun x -> x))) ~expected:[];
    t "singleton" ~actual:S.(to_list (generate ~len:1 (fun x -> x))) ~expected:[0];
    t "small" ~actual:S.(to_list (generate ~len:3 (fun x -> x))) ~expected:[0; 1; 2];
  ];
  let t = T.test T.(list int) ~verbose in
  T.group "Stream.repeat" [
    t "empty" ~actual:S.(to_list (repeat ~times:0 42)) ~expected:[];
    t "singleton" ~actual:S.(to_list (repeat ~times:1 42)) ~expected:[42];
    t "multiple" ~actual:S.(to_list (repeat ~times:3 0)) ~expected:[0; 0; 0];
    t "take infinite" ~actual:S.(to_list (take 3 (repeat 0))) ~expected:[0; 0; 0];
  ];
  T.group "Stream.repeatedly" [
    t "empty" ~actual:S.(to_list (repeatedly ~times:0 (fun () -> 42))) ~expected:[];
    t "singleton" ~actual:S.(to_list (repeatedly ~times:1 (fun () -> 42))) ~expected:[42];
    t "multiple" ~actual:S.(to_list (repeatedly ~times:3 (fun () -> 0))) ~expected:[0; 0; 0];
    t "take infinite" ~actual:S.(to_list (take 3 (repeatedly (fun () -> 0)))) ~expected:[0; 0; 0];
  ];
  let t = T.test T.(list int) ~verbose in
  T.group "Stream.iterate" [
    t "empty"
      ~actual:S.(to_list (take 0 (iterate 2 (fun x -> x * 2)))) ~expected:[];
    t "take 4"
      ~actual:S.(to_list (take 4 (iterate 2 (fun x -> x * 2)))) ~expected:[2; 4; 8; 16];
  ];
  let t = T.test T.(list int) ~verbose in
  T.group "Stream.unfold" [
    t "infinite, take 0" ~expected:[]
      ~actual:S.(to_list (take 0 (unfold 0 (fun x -> Some (x, x+1)))));
    t "infinite, take 2" ~expected:[0; 1]
      ~actual:S.(to_list (take 2 (unfold 0 (fun x -> Some (x, x+1)))));
    t "finite" ~expected:[0; 1]
      ~actual:S.(to_list (unfold 0 (function 2 -> None | x -> Some (x, x+1))));
  ];


  let t = T.test T.(list int) ~verbose in
  T.group "Stream.map" [
    t "empty" ~expected:[]
      ~actual:S.(to_list (map (fun x -> x) empty));
    t "small" ~expected:[0; 1; 2]
      ~actual:S.(to_list (map int_of_string (S.of_list ["0"; "1"; "2"])));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Stream.filter" [
    t "empty" ~expected:[]
      ~actual:S.(to_list (filter (fun _ -> true) empty));
    t "filter none" ~expected:[]
      ~actual:S.(to_list (filter (fun _ -> false) (S.of_list [0; 1; 2])));
    t "filter all" ~expected:[0; 1; 2]
      ~actual:S.(to_list (filter (fun _ -> true) (S.of_list [0; 1; 2])));
    t "filter even" ~expected:[0; 2; 4]
      ~actual:S.(to_list (filter (fun x -> x mod 2 = 0) (S.of_list [0; 1; 2; 4])));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Stream.take" [
    t "take 0, empty" ~expected:[]
      ~actual:S.(to_list (take 0 empty));
    t "take 2, empty" ~expected:[]
      ~actual:S.(to_list (take 2 empty));
    t "take 0, small" ~expected:[]
      ~actual:S.(to_list (take 0 (S.of_list [1; 2; 3])));
    t "take 2, small" ~expected:[1; 2]
    ~actual:S.(to_list (take 2 (S.of_list [1; 2; 3])));
    t "take 0, infinite" ~expected:[]
    ~actual:S.(to_list (take 0 (S.count 0)));
    t "take 2, infinite" ~expected:[0; 1]
    ~actual:S.(to_list (take 2 (S.count 0)));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Stream.take_while" [
    t "all, empty" ~expected:[]
      ~actual:S.(to_list (take_while (fun _ -> true) empty));
    t "none, empty" ~expected:[]
      ~actual:S.(to_list (take_while (fun _ -> false) empty));
    t "some, infinite" ~expected:[0; 1; 2]
      ~actual:S.(to_list (take_while (fun x -> x < 3) (count 0)));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Stream.drop" [
    t "none, empty" ~expected:[]
      ~actual:S.(to_list (drop 0 empty));
    t "some, empty" ~expected:[]
      ~actual:S.(to_list (drop 3 empty));
    t "none, finite" ~expected:[0; 1; 2]
    ~actual:S.(to_list (drop 0 (of_list [0; 1; 2])));
    t "some, finite" ~expected:[2; 3]
    ~actual:S.(to_list (drop 2 (of_list [0; 1; 2; 3])));
    t "all, finite" ~expected:[]
    ~actual:S.(to_list (drop 3 (of_list [0; 1; 2])));
    t "more, finite" ~expected:[]
    ~actual:S.(to_list (drop 5 (of_list [0; 1; 2])));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Stream.drop_while" [
    t "none, empty" ~expected:[]
      ~actual:S.(to_list (drop_while (fun _ -> false) empty));
    t "all, empty" ~expected:[]
      ~actual:S.(to_list (drop_while (fun _ -> true) empty));
    t "none, finite" ~expected:[0; 1; 2]
    ~actual:S.(to_list (drop_while (fun _ -> false) (of_list [0; 1; 2])));
    t "some, finite" ~expected:[2; 3]
    ~actual:S.(to_list (drop_while (fun x -> x < 2) (of_list [0; 1; 2; 3])));
    t "all, finite" ~expected:[]
    ~actual:S.(to_list (drop_while (fun _ -> true) (of_list [0; 1; 2])));
  ];

  let t = T.test T.(list (pair int int)) ~verbose in
  T.group "Stream.indexed" [
    t "empty" ~expected:[] ~actual:S.(to_list (indexed empty));
    t "single" ~expected:[(0, 10)] ~actual:S.(to_list (indexed (single 10)));
    t "multiple" ~expected:[(0, 10); (1, 11); (2, 12)]
      ~actual:S.(to_list (indexed (10-<13)));
  ];

  let t = T.test T.int ~verbose in
  T.group "Stream.fold" [
    t "empty" ~expected:0 ~actual:(S.fold (+) 0 S.empty);
    t "small" ~expected:3 ~actual:(S.fold (+) 0 (S.of_list [0; 1; 2]));
    T.test T.(pair int int) ~verbose "emtpy termination" ~expected:(2, 0)
      ~actual:begin
        let n = ref 0 in
        let src = Source.make ~init:(fun () -> incr n; 0) ~stop:(fun _ -> incr n)
          ~pull:(fun _ -> None) () in
        !n, S.fold (+) 0 (S.from src)
      end;
    T.test T.(pair int int) ~verbose "finite termination" ~expected:(2, 3)
      ~actual:begin
        let n = ref 0 in
        let src = Source.make ~init:(fun () -> incr n; 0) ~stop:(fun _ -> incr n)
          ~pull:(function 3 -> None | x -> Some (x, x + 1)) () in
        !n, S.fold (+) 0 (S.from src)
      end
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Stream.concat" [
    t "both empty" ~expected:[]
      ~actual:S.(to_list (concat empty empty));
    t "left empty" ~expected:[0; 1; 2]
      ~actual:S.(to_list (concat empty (0-<3)));
    t "right empty" ~expected:[0; 1; 2]
      ~actual:S.(to_list (concat (0-<3) empty));
    t "both non-empty" ~expected:[0; 1; 2; 3; 4; 5]
      ~actual:S.(to_list (concat (0-<3) (3-<6)));
    t "take, left infinite" ~expected:[0; 0; 0; 0]
      ~actual:S.(to_list (take 4 (concat (repeat 0) (0-<3))));
    t "take, right infinite" ~expected:[0; 1; 2; 0]
      ~actual:S.(to_list (take 4 (concat (0-<3) (repeat 0))));
    t "take, both infinite" ~expected:[0; 0; 0; 0]
      ~actual:S.(to_list (take 4 (concat (repeat 0) (repeat 1))));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Stream.flatten" [
    t "empty" ~expected:[]
      ~actual:S.(to_list (flatten empty));
    t "two empty" ~expected:[]
      ~actual:S.(to_list (flatten (of_list [empty; empty])));
    t "left empty" ~expected:[0; 1; 2]
      ~actual:S.(to_list (flatten (of_list [0-<3; empty])));
    t "right empty" ~expected:[0; 1; 2]
      ~actual:S.(to_list (flatten (of_list [empty; 0-<3])));
    t "two" ~expected:[0; 1; 2; 3; 4; 5]
      ~actual:S.(to_list (flatten (of_list [0-<3; 3-<6])));
    t "three" ~expected:[0; 1; 2; 3; 4; 5]
      ~actual:S.(to_list (flatten (of_list [0-<2; 2-<4; 4-<6])));
    t "take, left infinite" ~expected:[0; 0; 0]
      ~actual:S.(to_list (take 3 (flatten (of_list [repeat 0; 0-<6]))));
    t "take, right infinite" ~expected:[0; 1; 2]
      ~actual:S.(to_list (take 3 (flatten (of_list [0-<6; repeat 0]))));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Stream.flat_map" [
    t "empty inner and outer" ~expected:[]
      ~actual:S.(to_list (flat_map (fun _ -> empty) empty));
    t "empty inner" ~expected:[]
      ~actual:S.(to_list (flat_map (fun _ -> empty) (0-<10)));
    t "empty outer" ~expected:[]
      ~actual:S.(to_list (flat_map (fun _ -> (0-<5)) empty));
    t "single inner, empty outer" ~expected:[]
      ~actual:S.(to_list (flat_map single empty));
    t "single inner, single outer" ~expected:[42]
      ~actual:S.(to_list (flat_map single (single 42)));
    t "single inner, double outer" ~expected:[1; 2]
      ~actual:S.(to_list (flat_map single (double 1 2)));
    t "duplicate outer" ~expected:[0; 0; 1; 1; 2; 2]
      ~actual:S.(to_list (flat_map (fun x -> double x x) (0-<3)));
    t "infinite inner, empty outer" ~expected:[]
      ~actual:S.(to_list (flat_map repeat empty));
    t "infinite inner, single outer, take" ~expected:[0; 0; 0]
      ~actual:S.(to_list (take 3 (flat_map repeat (single 0))));
    t "single inner, infinite outer, take" ~expected:[0; 0; 0]
      ~actual:S.(to_list (take 3 (flat_map single (repeat 0))));

    (* Results in infinite loop. Seems to be unavoidable. *)
    (* t "empty inner, infinite outer, take" ~expected:[] *)
    (*   ~actual:S.(to_list (take 3 (flat_map (fun _ -> empty) (repeat 0)))) *)
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Stream.cycle" [
    t "single, take" ~expected:[0; 0; 0]
      ~actual:S.(to_list (take 3 (cycle (single 0))));
    t "single, times" ~expected:[0; 0; 0]
      ~actual:S.(to_list (cycle ~times:3 (single 0)));
    t "range, times" ~expected:[0; 1; 0; 1; 0; 1]
      ~actual:S.(to_list (cycle ~times:3 (0-<2)));
    t "range, take" ~expected:[0; 1; 0; 1]
      ~actual:S.(to_list (take 4 (cycle (0-<2))));
    t "infinite, take" ~expected:[0; 0; 0; 0]
      ~actual:S.(to_list (take 4 (cycle (repeat 0))));
    t "nested, take" ~expected:[0; 0; 0; 0]
      ~actual:S.(to_list (take 4 (cycle (cycle (single 0)))));
    (* Results in infinite loop. Seems to be unavoidable. *)
    (* t "empty, take" ~expected:[] *)
    (*   ~actual:S.(to_list (take 3 (cycle empty))) *)
  ];

  let t = T.test T.(list (list int)) ~verbose in
  T.group "Stream.partition" [
    t "size 0, empty" ~expected:[]
      ~actual:S.(to_list (map to_list (partition 0 empty)));
    t "size 0, non-empty" ~expected:[]
      ~actual:S.(to_list (map to_list (partition 0 (0-<5))));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Stream.interpose" [
    t "empty" ~expected:[]
      ~actual:S.(to_list (interpose 0 empty));
    t "single" ~expected:[1]
      ~actual:S.(to_list (interpose 0 (single 1)));
    t "double" ~expected:[1; 0; 2]
      ~actual:S.(to_list (interpose 0 (double 1 2)));
    t "range" ~expected:[5; 0; 6; 0; 7; 0; 8]
      ~actual:S.(to_list (interpose 0 (5-<9)));
  ];




