module type SET =
  sig
    type elem
    type set
           
    val empty : set
    val insert : elem -> set -> set
    val member : elem -> set -> bool
                                  
    val complete : elem -> int -> set
    val balanced : elem -> int -> set
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

    (* Exercise 2.2 *)
    let member x s =
      let rec go x s last =
        match s, last with
        | E, None -> false
        | E, Some v -> (Element.compare x v) = 0
        | T (a, y, b), last ->
           if (Element.compare x y) < 0 then go x a last
           else go x b (Some y)
      in
      go x s None

    (* Exercise 2.3 and 2.4 *)
    exception NoCopying
    let rec insert x s =
      let rec go x s last =
        match s, last with
        | E, None -> T(E, x, E)
        | E, Some v -> 
           if (Element.compare x v) <> 0 then T(E, x, E) 
           else raise NoCopying
        | T (a, y, b), last ->
           try
             if (Element.compare x y) < 0 then T (go x a last, y, b)
             else T (a, y, go x b (Some y))
           with
             NoCopying -> s
      in
      go x s None

    (* Exercise 2.5a *)
    let rec complete x d =
      if d = 0 then E
      else
        let s = complete x (d-1) in
        T(s, x, s)

    (* Exercise 2.5b *)
    let rec balanced x n =
      let rec create2 m =
        if m = 0 then (E, T (E, x, E))
        else 
          let t1, t2 = create2 ((m-1)/2) in
          if m mod 2 = 1 then (T (t1, x, t1), T (t1, x, t2))
          else (T (t1, x, t2), T (t2, x, t2))
      in
      if n = 0 then E
      else if n mod 2 = 1 then
        let s = balanced x (n/2) in
        T (s, x, s)
      else
        let t1, t2 = create2 ((n-1)/2) in
        T (t1, x, t2)
  end
