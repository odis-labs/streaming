open Streaming
module T = Nanotest
let (-<) = Stream.(-<)
let into = Stream.into


let () =
  let verbose = true in

  let t = T.test T.int ~verbose ~expected:42 in
  T.group "Sink.fill" [
    t "no input" ~actual:(into (Sink.fill 42) Stream.empty);
    t "finite input" ~actual:(into (Sink.fill 42) (0-<10));
    t "infinite input" ~actual:(into (Sink.fill 42) (Stream.repeat 0));
  ];

  let t = T.test T.unit ~verbose ~expected:() in
  T.group "Sink.full" [
    t "no input" ~actual:(into Sink.full Stream.empty);
    t "finite input" ~actual:(into Sink.full (0-<10));
    t "infinite input" ~actual:(into Sink.full (Stream.repeat 0));
  ];

  let t = T.test T.int ~verbose in
  T.group "Sink.len" [
    t "no input" ~actual:(into Sink.len Stream.empty) ~expected:0;
    t "finite input" ~actual:(into Sink.len (0-<10)) ~expected:10;
  ];

  let t = T.test T.int ~verbose in
  T.group "Sink.drain" [
    t "no input" ~actual:(into Sink.len Stream.empty) ~expected:0;
    t "finite input" ~expected:10 ~actual:begin
      let n = ref 0 in
      Stream.generate ~len:10 (fun i -> incr n; i)
      |> into Sink.drain;
      !n
    end
  ];

  let t = T.test T.int ~verbose in
  T.group "Sink.each" [
    t "no input" ~actual:(into Sink.len Stream.empty) ~expected:0;
    t "finite input" ~expected:10 ~actual:begin
      let n = ref 0 in
      Stream.generate ~len:10 (fun i -> i)
      |> into (Sink.each (fun _ -> incr n));
      !n
    end
  ];

  let t = T.test T.int ~verbose in
  T.group "Sink.fold" [
    t "no input" ~actual:(into (Sink.fold (+) 0) Stream.empty) ~expected:0;
    t "finite input" ~actual:(into (Sink.fold (+) 0) (0-<10)) ~expected:45;
  ];

  let t = T.test T.bool ~verbose in
  T.group "Sink.is_empty" [
    t "no input" ~actual:(into Sink.is_empty Stream.empty) ~expected:true;
    t "finite input" ~actual:(into Sink.is_empty (0-<10)) ~expected:false;
  ];

  let t = T.test ~verbose T.(option int) in
  T.group "Sink.nth" [
    t "nth 0, finite input" ~actual:(into (Sink.nth 0) (0-<10)) ~expected:(Some 0);
    t "nth 2, no input" ~actual:(into (Sink.nth 2) Stream.empty) ~expected:None;
    t "nth 2, finite input" ~actual:(into (Sink.nth 2) (0-<10)) ~expected:(Some 2);
  ];

  let t = T.test ~verbose T.(option int) in
  T.group "Sink.first" [
    t "no input" ~actual:(into Sink.first Stream.empty) ~expected:None;
    t "finite input" ~actual:(into Sink.first (0-<10)) ~expected:(Some 0);
  ];

  let t = T.test ~verbose T.(option int) in
  T.group "Sink.last" [
    t "no input" ~actual:(into Sink.last Stream.empty) ~expected:None;
    t "finite input" ~actual:(into Sink.last (0-<10)) ~expected:(Some 9);
  ];

  let t = T.test ~verbose T.bool in
  T.group "Sink.contains" [
    t "no input" ~actual:(into (Sink.contains ~where:((=) 5)) Stream.empty) ~expected:false;
    t "must find, finite input"
      ~actual:(into (Sink.contains ~where:((=) 5)) (0-<10)) ~expected:true;
    t "must not find, finite input"
      ~actual:(into (Sink.contains ~where:((=) 100)) (0-<10)) ~expected:false;
    t "must find, infinite input"
      ~actual:(into (Sink.contains ~where:((=) 2)) (Stream.cycle (0-<5))) ~expected:true;
  ];

  let t = T.test ~verbose T.(option int) in
  T.group "Sink.find" [
    t "no input" ~actual:(into (Sink.find ~where:((=) 5)) Stream.empty) ~expected:None;
    t "must find, finite input"
      ~actual:(into (Sink.find ~where:((=) 5)) (0-<10)) ~expected:(Some 5);
    t "must not find, finite input"
      ~actual:(into (Sink.find ~where:((=) 100)) (0-<10)) ~expected:None;
    t "must find, infinite input"
      ~actual:(into (Sink.find ~where:((=) 2)) (Stream.cycle (0-<5))) ~expected:(Some 2);
  ];

  let t = T.test ~verbose T.(option int) in
  T.group "Sink.index" [
    t "no input"
      ~actual:(into (Sink.index ~where:((=) 'y')) Stream.empty)
      ~expected:None;
    t "must find, finite input"
      ~actual:(into (Sink.index ~where:((=) 'y')) (Stream.of_list ['x'; 'y'; 'z']))
      ~expected:(Some 1);
    t "must not find, finite input"
      ~actual:(into (Sink.index ~where:((=) 'w')) (Stream.of_list ['x'; 'y'; 'z']))
      ~expected:None;
    t "must find, infinite input"
      ~actual:(into (Sink.index ~where:((=) 'y')) (Stream.cycle (Stream.of_list ['x'; 'y'; 'z'])))
      ~expected:(Some 1);
  ];

  let t = T.test ~verbose T.(option int) in
  T.group "Sink.minimum" [
    t "no input" ~actual:(into (Sink.minimum ~by:(<)) Stream.empty) ~expected:None;
    t "must find, finite input"
      ~actual:(into (Sink.minimum ~by:(<)) (0-<10)) ~expected:(Some 0);
  ];

  let t = T.test ~verbose T.(option int) in
  T.group "Sink.maximum" [
    t "no input" ~actual:(into (Sink.maximum ~by:(>)) Stream.empty) ~expected:None;
    t "must find, finite input"
      ~actual:(into (Sink.maximum ~by:(>)) (0-<10)) ~expected:(Some 9);
  ];

  let t = T.test ~verbose T.bool in
  T.group "Sink.all" [
    t "no input" ~actual:(into (Sink.all ~where:(fun x -> x > 5)) Stream.empty) ~expected:true;
    t "must be false, finite input"
      ~actual:(into (Sink.all ~where:(fun x -> x > 5)) (0-<10)) ~expected:false;
    t "must be true, finite input"
      ~actual:(into (Sink.all ~where:(fun x -> x > 5)) (6-<20)) ~expected:true;
    t "must be false, infinite input"
      ~actual:(into (Sink.all ~where:(fun x -> x < 10)) (Stream.cycle (0-<20))) ~expected:false;
  ];

  let t = T.test ~verbose T.bool in
  T.group "Sink.any" [
    t "no input" ~actual:(into (Sink.any ~where:(fun x -> x > 5)) Stream.empty) ~expected:false;
    t "must be false, finite input"
      ~actual:(into (Sink.any ~where:(fun x -> x > 5)) (0-<10)) ~expected:true;
    t "must be true, finite input"
      ~actual:(into (Sink.any ~where:(fun x -> x = 5)) (0-<10)) ~expected:true;
    t "must be true, infinite input"
      ~actual:(into (Sink.any ~where:(fun x -> x = 5)) (Stream.cycle (0-<10))) ~expected:true;
  ];

  let t = T.test ~verbose T.(list int) in
  T.group "Sink.list" [
    t "no input" ~actual:(into Sink.list Stream.empty) ~expected:[];
    t "finite input" ~actual:(into Sink.list (0-<5)) ~expected:[0; 1; 2; 3; 4];
    t "infinite input with take" ~actual:(into Sink.list (Stream.(take 3 (repeat 0)))) ~expected:[0; 0; 0];
  ];

  let t = T.test ~verbose T.(array int) in
  T.group "Sink.array" [
    t "no input" ~actual:(into Sink.array Stream.empty) ~expected:[||];
    t "finite input" ~actual:(into Sink.array (0-<5)) ~expected:[|0; 1; 2; 3; 4|];
  ];

  let t = T.test ~verbose T.(array int) in
  T.group "Sink.buffer" [
    t "empty buffer, no input" ~actual:(into (Sink.buffer 0) Stream.empty) ~expected:[||];
    T.raises "must fail on negative size"
      ~exn:(Invalid_argument "Streaming.Sink.buffer: negative buffer size")
      (fun () -> Sink.buffer (-1));
    t "size 4, no input" ~actual:(into (Sink.buffer 4) Stream.empty) ~expected:[||];
    t "size 4, input 3" ~actual:(into (Sink.buffer 4) (0-<3)) ~expected:[|0; 1; 2|];
    t "size 4, input 5" ~actual:(into (Sink.buffer 4) (0-<5)) ~expected:[|0; 1; 2; 3|];
    t "size 2, input 20" ~actual:(into (Sink.buffer 2) (0-<20)) ~expected:[|0; 1|];
  ];

  let t = T.test ~verbose T.int in
  T.group "Sink.sum" [
    t "no input" ~actual:(into Sink.sum Stream.empty) ~expected:0;
    t "finite input" ~actual:(into Sink.sum (0-<5)) ~expected:10;
  ];

  let t = T.test ~verbose T.int in
  T.group "Sink.product" [
    t "no input" ~actual:(into Sink.product Stream.empty) ~expected:1;
    t "finited input" ~actual:(into Sink.product (Stream.of_list [1; 4; 2])) ~expected:8;
    t "finite input with zero" ~actual:(into Sink.product (Stream.of_list [1; 0; 2])) ~expected:0;
  ];

  let t = T.test ~verbose T.string in
  T.group "Sink.string" [
    t "no input" ~actual:(into Sink.string Stream.empty) ~expected:"";
    t "finited input" ~actual:(into Sink.string (Stream.of_list ["a"; "b"; "c"])) ~expected:"abc";
  ];

  let t = T.test ~verbose T.int in
  T.group "Sink.dispose" [
    t "init and stop" ~expected:2 ~actual:begin
      let x = ref 0 in
      let sink = Sink.make ~init:(fun () -> incr x) ~stop:(fun () -> incr x)
        ~push:(fun r _ -> r) () in
      Sink.dispose sink;
      !x
    end;
  ];

  let t = T.test ~verbose T.int in
  T.group "Sink.map" [
    t "no input" ~expected:0
      ~actual:(into Sink.(map String.length string) Stream.empty) ;
    t "finite" ~expected:3
      ~actual:(into Sink.(map String.length string) (Stream.of_list ["a"; "b"; "c"]));
  ];

  let t = T.test ~verbose T.(pair int int) in
  T.group "Sink.zip" [
    t "no input" ~expected:(0, 0) ~actual:(into Sink.(zip len sum) Stream.empty);
    t "finite" ~expected:(5, 35) ~actual:(into Sink.(zip len sum) (5-<10));
  ];

  let t = T.test ~verbose T.(pair (list int) (list int)) in
  T.group "Sink.distribute" [
    t "no input" ~expected:([], []) ~actual:(into Sink.(distribute list list) Stream.empty);
    t "finite" ~expected:([0; 2; 4], [1; 3; 5])
      ~actual:(into Sink.(distribute list list) (0-<6));
  ];

  let t = T.test ~verbose T.(pair (list int) (list int)) in
  T.group "Sink.unzip" [
    t "no input" ~expected:([], []) ~actual:(into Sink.(unzip list list) Stream.empty);
    t "finite" ~expected:([1; 2; 3], [10; 20; 30])
      ~actual:(into Sink.(unzip list list) (Stream.of_list [(1, 10); (2, 20); (3, 30)]));
  ];

  let t = T.test ~verbose T.float in
  T.group "Sink.mean" [
    t "no input" ~expected:0.0 ~actual:(into Sink.mean Stream.empty);
    t "finite small" ~expected:2.0 ~actual:(into Sink.mean (Stream.of_list [1.0; 2.0; 3.0]));
    t "finite zeros" ~expected:0.0 ~actual:(into Sink.mean (Stream.of_list [0.0; 0.0; 0.0]));
  ];

