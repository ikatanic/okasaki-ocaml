exception Empty

module type QUEUE =
  sig
    type 'a queue

    val empty : 'a queue
    val isEmpty : 'a queue -> bool

    val snoc : 'a queue -> 'a -> 'a queue
    val head : 'a queue -> 'a (* raises Empty if queue is empty *)
    val tail : 'a queue -> 'a queue (* raises Empty if queue is empty *)
  end

module BatchedQueue : QUEUE =
  struct
    type 'a queue = 'a list * 'a list

    let empty = ([], [])
    let isEmpty (f, _) = f = []

    let checkf = function
      | [], r -> (List.rev r, [])
      | q -> q

    let snoc (f, r) x = checkf (f, x::r)
    let head = function
      | [], _ -> raise Empty
      | x::f, _ -> x
    let tail = function
      | [], _ -> raise Empty
      | x::f, r -> checkf (f, r)
  end
    
    
    
(* Exercise 5.1 *)
module type DEQUE =
  sig
    type 'a deque

    val empty : 'a deque
    val isEmpty : 'a deque -> bool

    (* insert, inspect and remove the front element *)
    val cons : 'a -> 'a deque -> 'a deque
    val head : 'a deque -> 'a (* raises Empty if queue is empty *)
    val tail : 'a deque -> 'a deque (* raises Empty if queue is empty *)

    (* insert, inspect and remove the rear element *)
    val snoc : 'a deque -> 'a -> 'a deque
    val last : 'a deque -> 'a (* raises Empty if queue is empty *)
    val init : 'a deque -> 'a deque (* raises Empty if queue is empty *)
  end

module BatchedDeque : DEQUE =
  struct
    type 'a deque = 'a list * 'a list

    let empty = ([], [])
    let isEmpty (f, r) = f = [] && r = []

    let rec split l n acc =
      if n = 0 then (List.rev acc, l)
      else match l with
           | hd::tl -> split tl (n-1) (hd::acc)
           | [] -> raise Empty
                         
    let check = function
      | [], r ->
         let a, b = split r ((List.length r) / 2) [] in
         (List.rev b, a)
      | f, [] ->
         let a, b = split f ((List.length f) / 2) [] in
         (a, List.rev b)
      | d -> d
                      
    let cons x (f, r) = check (x::f, r)
    let head = function
      | x::f, r -> x
      | [], x::r -> x
      | _ -> raise Empty
    let tail = function
      | x::f, r -> check (f, r)
      | [], x::r -> check ([], r)
      | _ -> raise Empty

    let snoc (f, r) x = check (f, x::r)
    let last = function
      | f, x::r -> x
      | x::f, [] -> x
      | _ -> raise Empty
    let init = function
      | f, x::r -> check (f, r)
      | x::f, [] -> check (f, [])
      | _ -> raise Empty
  end
