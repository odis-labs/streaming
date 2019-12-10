

module type Functor = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end


module Codensity (F : Functor) = struct
  type ('a, 'r) t = ('a -> 'r F.t) -> 'r F.t

  let bind (m : ('a, 'r) t) k : ('b, 'r) t =
    fun c -> m (fun a -> k a c)
end


type ('a, 's) step =
  | Yield of 'a * 's
  | Empty

type 'a src =
  Src : ('s * ('s -> ('a, 's) step)) -> 'a src


module Codensity(M : sig type 'a t end) = struct
  type 'a t =
    { run : 'z . ('a -> 'z M.t) -> 'z M.t }
  
  let bind g f = { run = fun c -> f.run (fun x -> (g x).run c) }
end

module Ksrc = struct
  type 'a t = Src : ('s * ('s -> ('a * 's) option)) -> 'a t

  module C = Codensity(struct type nonrec 'a t = 'a t end)

  let bind = C.bind

end


module Source = struct
  module K = struct
    type ('a, 's) k = {
      init : 's;
      step : 'r . ('a -> 's -> 'r) -> 's -> 'r -> 'r;
    }

    type 'a t = K : ('a, 's) k -> 'a t

    let generate n f =
      let step k i r =
        if i = n then r
        else k (f i) (i + 1) in
      K { init = 0; step }


    let fold f r0 (K self) =
      let rec go s r =
        self.step (fun a s' -> go s' (f r a)) s r in 
      go self.init r0


    let map f (K self) =
      let step k s = self.step (fun x s' -> k (f x) s') s in
      K { self with step }

    let empty = K { init = (); step = (fun _k _s r -> r) }
  end
end

