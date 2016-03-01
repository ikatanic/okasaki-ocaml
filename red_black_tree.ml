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


module RedBlackTree (Element:ORDERED) : (SET with type elem = Element.t) =
  struct    
    type elem = Element.t
    type color = R | B
    type tree = E | T of color * tree * elem * tree
    type set = tree
                 
    let empty = E

    let balance = function
      | (B, T (R, T (R, a, x, b), y, c), z, d)
      | (B, T (R, a, x, T (R, b, y, c)), z, d)
      | (B, a, x, T (R, T (R, b, y, c), z, d))
      | (B, a, x, T (R, b, y, T (R, c, z, d))) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
      | color, a, x, b -> T (color, a, x, b)

    let rec insert x t =
      let rec ins = function
        | E -> T (R, E, x, E)
        | T (color, a, y, b) as s ->
           if Element.compare x y < 0 then balance (color, ins a, y, b)
           else if Element.compare x y > 0 then balance (color, a, y, ins b)
           else s
      in
      let T (_, a, y, b) = ins t in
      T (B, a, y, b)

    let rec member x = function
      | E -> false
      | T (_, a, y, b) ->
         if Element.compare x y < 0 then member x a
         else if Element.compare x y > 0 then member x b
         else true

    (* Exercise 3.9 - converting sorted list with no dupplicates 
        into a rb tree in O(n) time *)
    let fromOrdList l =
      let a = Array.of_list l in
      let rec go l r =
        if l >= r then E
        else if l+1 = r then T (R, E, a.(l), E)
        else
          let m = (l + r) / 2 in
          T (B, go l m, a.(m), go (m+1) r)
      in go 0 (Array.length a);
  end
