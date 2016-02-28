module type ORDERED = 
  (* a totally ordered type and it's comparison function *)
  sig
    type t
    val compare : t -> t -> int
  end

exception Empty

module type HEAP =
  sig
    module Elem : ORDERED
                    
    type heap

    val empty : heap
    val isEmpty : heap -> bool

    val insert : Elem.t -> heap -> heap
    val merge : heap -> heap -> heap

    val findMin : heap -> Elem.t (* raises Empty if heap is empty *)
    val deleteMin : heap -> heap (* raises Empty if heap is empty *)
  end


module LeftistHeap (Element:ORDERED) : (HEAP with module Elem = Element) =
  struct
    module Elem = Element
          
    type heap = E | T of int * Elem.t * heap * heap
                                                 
    let empty = E
    let isEmpty = function
      | E -> true
      | _ -> false
               
    let rank = function
      | E -> 0
      | T (r, _, _, _) -> r
                            
    let makeT x a b =
      if rank a >= rank b then T (rank b + 1, x, a, b)
      else T (rank a + 1, x, b, a)

    let rec merge h1 h2 =
      match h1, h2 with
      | E, h -> h
      | h, E -> h
      | T (_, x, a, b), T (_, y, c, d) ->
         if Elem.compare x y < 0 then
           makeT x a (merge b h2)
         else
           makeT y c (merge d h1)

    let insert x = merge (T (1, x, E, E))

    let findMin = function
      | E -> raise Empty
      | T (_, x, _, _) -> x

    let deleteMin = function
      | E -> raise Empty
      | T (_, _, a, b) -> merge a b

    (* Exercise 3.3 *)
    let fromList a =
      let rec build xs =
        let rec join = function
          | [] -> []
          | [hd] -> [hd]
          | h1::h2::tl -> (merge h1 h2)::join tl
        in
        match xs with
        | [] -> assert false
        | [hd] -> hd
        | _ -> build (join xs)
      in
      build (List.map (fun x -> T (1, x, E, E)) a)
  end



(* Exercise 3.4b *)
module WeightBiasedHeap (Element:ORDERED) : (HEAP with module Elem = Element) =
  struct
    module Elem = Element
          
    type heap = E | T of int * Elem.t * heap * heap
                                                 
    let empty = E
    let isEmpty = function
      | E -> true
      | _ -> false
               
    let size = function
      | E -> 0
      | T (s, _, _, _) -> s
                            
    let makeT x a b =
      if size a >= size b then T (size a + size b + 1, x, a, b)
      else T (size a + size b + 1, x, b, a)

    let rec merge h1 h2 =
      match h1, h2 with
      | E, h -> h
      | h, E -> h
      | T (_, x, a, b), T (_, y, c, d) ->
         if Elem.compare x y < 0 then
           makeT x a (merge b h2)
         else
           makeT y c (merge d h1)

    let insert x = merge (T (1, x, E, E))

    let findMin = function
      | E -> raise Empty
      | T (_, x, _, _) -> x

    let deleteMin = function
      | E -> raise Empty
      | T (_, _, a, b) -> merge a b

    (* Exercise 3.4c *)
    let rec merge2 h1 h2 =
      match h1, h2 with
      | E, h -> h
      | h, E -> h
      | T (_, x, a, b), T (_, y, c, d) ->
         let nsize = size h1 + size h2 in
         if Elem.compare x y < 0 then
           if size a >= size b + size h2 then T (nsize, x, a, (merge b h2))
           else T (nsize, x, (merge b h2), a)
         else
           if size c >= size d + size h1 then T (nsize, y, c, (merge d h1))
           else T (nsize, y, (merge d h1), c)
  end
