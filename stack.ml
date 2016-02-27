exception Empty
exception Subscript

module type STACK =
  sig
    type 'a stack
            
    val empty : 'a stack
    val isEmpty : 'a stack -> bool

    val cons : 'a -> 'a stack -> 'a stack
    val head : 'a stack -> 'a (* raises Empty if stack is empty *)
    val tail : 'a stack -> 'a stack (* raises Empty if stack is empty *)
                              
    val update : 'a stack -> int -> 'a -> 'a stack
    val (++) : 'a stack -> 'a stack -> 'a stack
  end


module List : STACK =
  struct
    type 'a stack = 'a list

    let empty = []
    let isEmpty xs = xs = []
                          
    let cons x xs = x::xs
    let head = function
      | [] -> raise Empty
      | hd::_ -> hd
    let tail = function
      | [] -> raise Empty
      | _::tl -> tl

    let rec (++) xs ys =
      match xs with
      | [] -> ys
      | hd::tl -> hd::(tl ++ ys)

    let rec update xs i v =
      match xs, i with
      | [], _ -> raise Subscript
      | _::tl, 0 -> v::tl
      | hd::tl, i -> hd :: update tl (i-1) v
  end


module CustomStack : STACK =
  struct
    type 'a stack = Nil | Cons of 'a * 'a stack
                                          
    let empty = Nil
    let isEmpty = function
      | Nil -> true
      | _ -> false

    let cons hd tl = Cons (hd, tl)
    let head = function
      | Nil -> raise Empty
      | Cons(hd, _) -> hd
    let tail = function
      | Nil -> raise Empty
      | Cons(_, tl) -> tl

    let rec update xs i v =
      match xs, i with
      | Nil, _ -> raise Subscript
      | Cons (_, tl), 0 -> cons v tl
      | Cons (hd, tl), i -> cons hd (update tl (i-1) v)

    let rec (++) xs ys =
      match xs with
      | Nil -> ys
      | Cons (hd, tl) -> cons hd (tl ++ ys)
end


(* Exercise 2.1 *)
let rec suffixes xs =
  if List.isEmpty xs then List.empty
  else List.cons (List.head xs) (suffixes (List.tail xs))
                    
