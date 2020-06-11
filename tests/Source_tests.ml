open Streaming
module T = Nanotest


let () =
  let verbose = true in
  let to_list src = List.rev (Source.fold (fun xs x -> x :: xs) [] src) in

  let t = T.test T.int ~verbose in
  T.group "Source.empty" [
    t "empty len" ~actual:(Source.len Source.empty) ~expected:0;
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Source.list" [
    t "empty" ~actual:(to_list (Source.list [])) ~expected:[];
    t "singleton" ~actual:(to_list (Source.list [1])) ~expected:[1];
    t "small" ~actual:(to_list (Source.list [1; 2; 3])) ~expected:[1; 2; 3];
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Source.array" [
    t "empty" ~actual:(to_list (Source.array [||])) ~expected:[];
    t "singleton" ~actual:(to_list (Source.array [|1|])) ~expected:[1];
    t "small" ~actual:(to_list (Source.array [|1; 2; 3|])) ~expected:[1; 2; 3];
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Source.generate" [
    t "empty" ~actual:(to_list (Source.generate ~len:0 (fun x -> x))) ~expected:[];
    t "singleton" ~actual:(to_list (Source.generate ~len:1 (fun x -> x))) ~expected:[0];
    t "small" ~actual:(to_list (Source.generate ~len:3 (fun x -> x))) ~expected:[0; 1; 2];
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Source.count" [
    t "empty" ~actual:(to_list Source.(take 0 (count 0))) ~expected:[];
    t "take 4" ~actual:(to_list Source.(take 4 (count 5))) ~expected:[5; 6; 7; 8];
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Source.iterate" [
    t "empty"
      ~actual:(to_list Source.(take 0 (iterate 2 (fun x -> x * 2)))) ~expected:[];
    t "take 4"
      ~actual:(to_list Source.(take 4 (iterate 2 (fun x -> x * 2)))) ~expected:[2; 4; 8; 16];
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Source.unfold" [
    t "infinite, take 0" ~expected:[]
      ~actual:(to_list Source.(take 0 (unfold 0 (fun x -> Some (x, x+1)))));
    t "infinite, take 2" ~expected:[0; 1]
      ~actual:(to_list Source.(take 2 (unfold 0 (fun x -> Some (x, x+1)))));
    t "finite" ~expected:[0; 1]
      ~actual:(to_list Source.(unfold 0 (function 2 -> None | x -> Some (x, x+1))));
  ];

  let src0 = Source.empty in
  let src1 = Source.list [0; 1; 2] in
  let src2 = Source.list ['x'; 'y'; 'z'] in
  let src3 = Source.count 0 in
  let t = T.test T.(list (pair int char)) ~verbose in
  T.group "Source.zip" [
    t "empty" ~expected:[]
      ~actual:(to_list Source.(zip Source.empty Source.empty));
    t "equal len" ~expected:[(0, 'x'); (1, 'y'); (2, 'z')]
      ~actual:(to_list Source.(zip src1 src2));
    t "empty and small" ~expected:[]
      ~actual:(to_list Source.(zip src0 src2));
    t "small and empty" ~expected:[]
      ~actual:(to_list Source.(zip src1 src0));
    t "infinite and samll" ~expected:[(0, 'x'); (1, 'y'); (2, 'z')]
      ~actual:(to_list Source.(zip src3 src2));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Source.map" [
    t "empty" ~expected:[]
      ~actual:(to_list Source.(map (fun x -> x) Source.empty));
    t "small" ~expected:[0; 1; 2]
      ~actual:(to_list Source.(map int_of_string (list ["0"; "1"; "2"])));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Source.filter" [
    t "empty" ~expected:[]
      ~actual:(to_list Source.(filter (fun _ -> true) Source.empty));
    t "filter none" ~expected:[]
      ~actual:(to_list Source.(filter (fun _ -> false) (list [0; 1; 2])));
    t "filter all" ~expected:[0; 1; 2]
      ~actual:(to_list Source.(filter (fun _ -> true) (list [0; 1; 2])));
    t "filter even" ~expected:[0; 2; 4]
    ~actual:(to_list Source.(filter (fun x -> x mod 2 = 0) (list [0; 1; 2; 4])));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Source.take" [
    t "take 0, empty" ~expected:[]
      ~actual:(to_list Source.(take 0 Source.empty));
    t "take 2, empty" ~expected:[]
      ~actual:(to_list Source.(take 2 Source.empty));
    t "take 0, small" ~expected:[]
      ~actual:(to_list Source.(take 0 (Source.list [1; 2; 3])));
    t "take 2, small" ~expected:[1; 2]
    ~actual:(to_list Source.(take 2 (Source.list [1; 2; 3])));
    t "take 0, infinite" ~expected:[]
    ~actual:(to_list Source.(take 0 (Source.count 0)));
    t "take 2, infinite" ~expected:[0; 1]
    ~actual:(to_list Source.(take 2 (Source.count 0)));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Source.take_while" [
    t "all, empty" ~expected:[]
      ~actual:(to_list Source.(take_while (fun _ -> true) Source.empty));
    t "none, empty" ~expected:[]
      ~actual:(to_list Source.(take_while (fun _ -> false) Source.empty));
    t "some, infinite" ~expected:[0; 1; 2]
      ~actual:(to_list Source.(take_while (fun x -> x < 3) (Source.count 0)));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Source.drop" [
    t "none, empty" ~expected:[]
      ~actual:(to_list Source.(drop 0 Source.empty));
    t "some, empty" ~expected:[]
      ~actual:(to_list Source.(drop 3 Source.empty));
    t "none, finite" ~expected:[0; 1; 2]
    ~actual:(to_list Source.(drop 0 (Source.list [0; 1; 2])));
    t "some, finite" ~expected:[2; 3]
    ~actual:(to_list Source.(drop 2 (Source.list [0; 1; 2; 3])));
    t "all, finite" ~expected:[]
    ~actual:(to_list Source.(drop 3 (Source.list [0; 1; 2])));
    t "more, finite" ~expected:[]
    ~actual:(to_list Source.(drop 5 (Source.list [0; 1; 2])));
  ];

  let t = T.test T.(list int) ~verbose in
  T.group "Source.drop_while" [
    t "none, empty" ~expected:[]
      ~actual:(to_list Source.(drop_while (fun _ -> false) Source.empty));
    t "all, empty" ~expected:[]
      ~actual:(to_list Source.(drop_while (fun _ -> true) Source.empty));
    t "none, finite" ~expected:[0; 1; 2]
    ~actual:(to_list Source.(drop_while (fun _ -> false) (Source.list [0; 1; 2])));
    t "some, finite" ~expected:[2; 3]
    ~actual:(to_list Source.(drop_while (fun x -> x < 2) (Source.list [0; 1; 2; 3])));
    t "all, finite" ~expected:[]
    ~actual:(to_list Source.(drop_while (fun _ -> true) (Source.list [0; 1; 2])));
  ];

  let t = T.test T.int ~verbose in
  T.group "Source.fold" [
    t "empty" ~expected:0 ~actual:(Source.fold (+) 0 Source.empty);
    t "small" ~expected:3 ~actual:(Source.fold (+) 0 (Source.list [0; 1; 2]));
    T.test T.(pair int int) ~verbose "emtpy termination" ~expected:(2, 0) ~actual:begin
      let n = ref 0 in
      let src = Source.make ~init:(fun () -> incr n; 0) ~stop:(fun _ -> incr n)
        ~pull:(fun _ -> None) () in
      !n, Source.fold (+) 0 src
    end;
    T.test T.(pair int int) ~verbose "finite termination" ~expected:(2, 3) ~actual:begin
      let n = ref 0 in
      let src = Source.make ~init:(fun () -> incr n; 0) ~stop:(fun _ -> incr n)
        ~pull:(function 3 -> None | x -> Some (x, x + 1)) () in
      !n, Source.fold (+) 0 src
    end
  ];

  let t = T.test T.int ~verbose in
  T.group "Source.len" [
    t "empty" ~expected:0 ~actual:(Source.len Source.empty);
    t "small" ~expected:3 ~actual:(Source.len (Source.list [0; 1; 2]));
  ];

  let t = T.test T.int ~verbose in
  T.group "Source.each" [
    t "empty" ~expected:0 ~actual:begin
      let n = ref 0 in
      Source.each (fun _ -> incr n) Source.empty;
      !n
    end;
    t "small" ~expected:3 ~actual:begin
      let n = ref 0 in
      Source.each (fun _ -> incr n) (Source.list [0; 1; 2]);
      !n
    end;
    t "emtpy termination" ~expected:2 ~actual:begin
      let n = ref 0 in
      let src = Source.make ~init:(fun () -> incr n; 0) ~stop:(fun _ -> incr n)
        ~pull:(fun _ -> None) () in
      Source.each (fun _ -> assert false) src;
      !n
    end;
    t "finite termination" ~expected:5 ~actual:begin
      let n = ref 0 in
      let src = Source.make ~init:(fun () -> incr n; 0) ~stop:(fun _ -> incr n)
        ~pull:(function 3 -> None | x -> Some (x, x + 1)) () in
      Source.each (fun _ -> incr n) src;
      !n
    end
  ];

  let t = T.test ~verbose T.int in
  T.group "Source.dispose" [
    t "init and stop" ~expected:2 ~actual:begin
      let x = ref 0 in
      let src = Source.make ~init:(fun () -> incr x) ~stop:(fun () -> incr x)
        ~pull:(fun _ -> None) () in
      Source.dispose src;
      !x
    end;
  ];

