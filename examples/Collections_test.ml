open Collections


module List_sink = Reducible_extend(struct
  type 'a t = 'a list
  type 'a state = 'a t

  let init () = []
  let step s x = x :: s
  let full _ = false
  let stop s = List.rev s
end)


let () = begin
  assert (List_sink.one 42 = [42]);
  assert (List_sink.replicate 3 'x' = ['x'; 'x'; 'x']);
  assert (List_sink.of_list [] = []);
  assert (List_sink.of_list [1; 2; 3] = [1; 2; 3]);
end

