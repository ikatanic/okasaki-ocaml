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


module BinomialHeap (Element:ORDERED) : (HEAP with module Elem = Element) =
  struct
    module Elem = Element
                    
    type tree = Node of int * Elem.t * tree list
    type heap = tree list

    let empty = []
    let isEmpty = function
      | [] -> true
      | _ -> false

    let rank (Node (r, _, _)) = r
    let root (Node (_, x, _)) = x

    let link (Node (r, x1, l1) as t1) (Node (_, x2, l2) as t2) =
      if Elem.compare x1 x2 < 0 then
        Node (r + 1, x1, t2::l1)
      else
        Node (r + 1, x2, t1::l2)

    let rec insTree t h =
      match h with
      | [] -> [t]
      | hd::tl ->
         if rank t < rank hd then t::h
         else insTree (link hd t) tl

    let insert x = insTree (Node (0, x, []))

    let rec merge h1 h2 =
      match h1, h2 with
      | [], _ -> h2
      | _, [] -> h1
      | hd1::tl1, hd2::tl2 ->
         if rank hd1 < rank hd2 then hd1 :: (merge tl1 h2)
         else if rank hd2 < rank hd1 then hd2 :: (merge h1 tl2)
         else insTree (link hd1 hd2) (merge tl1 tl2)

    let rec removeMinTree = function
      | [] -> raise Empty
      | [hd] -> (hd, [])
      | hd::tl ->
         let t', ts' = removeMinTree tl in
         if root hd < root t' then (hd, tl) else (t', hd::ts')
                
    let findMin h = 
      let t, _ = removeMinTree h in root t

    let deleteMin h =
      let Node (_, _, ts'), ts = removeMinTree h in
      merge (List.rev ts') ts

    (* Exercise 3.5 *)
    let rec findMin2 = function
      | [] -> raise Empty
      | [Node (_, x, _)] -> x
      | Node (_, x, _) :: tl ->
         let y = findMin2 tl in
         if Elem.compare x y < 0 then x else y
  end


module BinomialHeap2 (Element:ORDERED) : (HEAP with module Elem = Element) =
  struct
    module Elem = Element
                    
    type tree = Node of Elem.t * tree list
    type heap = (int * tree) list

    let empty = []
    let isEmpty = function
      | [] -> true
      | _ -> false

    let root (Node (x, _)) = x

    let link (Node (x1, l1) as t1) (Node (x2, l2) as t2) =
      if Elem.compare x1 x2 < 0 then
        Node (x1, t2::l1)
      else
        Node (x2, t1::l2)

    let rec insTree r t h =
      match h with
      | [] -> [(r, t)]
      | (hdr, hd)::tl ->
         if r < hdr then (r, t)::h
         else insTree (r+1) (link hd t) tl

    let insert x = insTree 0 (Node (x, []))

    let rec merge h1 h2 =
      match h1, h2 with
      | [], _ -> h2
      | _, [] -> h1
      | (r1, t1)::tl1, (r2, t2)::tl2 ->
         if r1 < r2 then (r1, t1) :: (merge tl1 h2)
         else if r2 < r1 then (r2, t2) :: (merge h1 tl2)
         else insTree (r1+1) (link t1 t2) (merge tl1 tl2)

    let rec removeMinTree = function
      | [] -> raise Empty
      | [(r, t)] -> (t, [])
      | (r, t)::tl ->
         let t', ts' = removeMinTree tl in
         if root t < root t' then (t, tl) else (t', (r, t)::ts')
                
    let findMin h = 
      let t, _ = removeMinTree h in root t

    let deleteMin h =
      let Node (_, ts'), ts = removeMinTree h in
      let nt = List.mapi (fun i t -> (i, t)) (List.rev ts') in
      merge nt ts
  end
