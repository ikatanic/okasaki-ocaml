(* Exercise 2.2 - less calls to compare function in `member`, useful if compare is expensive
   Exercise 3.10 - faster balancing *)

module type SET =
  sig
    type elem
    type set
           
    val empty : set
    val insert : elem -> set -> set
    val member : elem -> set -> bool
  end


module type ORDERED = 
  (* a totally ordered type and it's comparison function *)
  sig
    type t
    val compare : t -> t -> int
  end


module RedBlackTree2 (Element:ORDERED) : (SET with type elem = Element.t) =
  struct    
    type elem = Element.t
    type color = R | B
    type tree = E | T of color * tree * elem * tree
    type set = tree
                 
    let empty = E

    let lbalance = function
      | (B, T (R, T (R, a, x, b), y, c), z, d)
      | (B, T (R, a, x, T (R, b, y, c)), z, d) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
      | color, a, x, b -> T (color, a, x, b)

    let rbalance = function
      | (B, a, x, T (R, T (R, b, y, c), z, d))
      | (B, a, x, T (R, b, y, T (R, c, z, d))) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
      | color, a, x, b -> T (color, a, x, b)

    let rec insert x t =
      let rec ins = function
        | E -> T (R, E, x, E)
        | T (color, a, y, b) as s ->
           if Element.compare x y < 0 then lbalance (color, ins a, y, b)
           else if Element.compare x y > 0 then rbalance (color, a, y, ins b)
           else s
      in
      let T (_, a, y, b) = ins t in
      T (B, a, y, b)

    let member x s =
      let rec go x s last =
        match s, last with
        | E, None -> false
        | E, Some v -> (Element.compare x v) = 0
        | T (_, a, y, b), last ->
           if Element.compare x y < 0 then go x a last
           else go x b (Some y)
      in
      go x s None
  end
