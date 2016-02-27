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


module UnbalancedSet (Element:ORDERED) : (SET with type elem = Element.t) =
  struct    
    type elem = Element.t
    type tree = E | T of tree * elem * tree
    type set = tree
                 
    let empty = E

    let rec insert x s =
      match s with
      | E -> T (E, x, E)
      | T (a, y, b) ->
         let cmp = Element.compare x y in
         if cmp < 0 then T (insert x a, y, b)
         else if cmp > 0 then T (a, y, insert x b)
         else s

    let rec member x s =
      match s with
      | E -> false
      | T (a, y, b) ->
         let cmp = Element.compare x y in
         if cmp = 0 then true
         else if cmp < 0 then member x a
         else member x b
  end
