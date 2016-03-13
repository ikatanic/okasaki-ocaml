module type STREAM =
  sig
    type 'a stream_cell = Nil | Cons of 'a * 'a stream
    and 'a stream = 'a stream_cell Lazy.t

    val (++) : 'a stream -> 'a stream -> 'a stream
    val take : int -> 'a stream -> 'a stream
    val drop : int -> 'a stream -> 'a stream
    val reverse : 'a stream -> 'a stream
  end

module Stream : STREAM =
  struct
    type 'a stream_cell = Nil | Cons of 'a * 'a stream
    and 'a stream = 'a stream_cell Lazy.t
                       
    let rec (++) a b = 
      lazy (
          match a with
          | lazy Nil -> Lazy.force b
          | lazy (Cons (hd, tl)) -> Cons (hd, tl ++ b)
        )

    let rec take n a =
      lazy (
          match n, a with
          | 0, _ -> Nil
          | _, lazy Nil -> Nil
          | _, lazy (Cons (hd, tl)) -> Cons (hd, take (n-1) tl)
        )

    let rec drop n a =
      lazy (
          match n, a with
          | 0, _ -> Lazy.force a
          | _, lazy Nil -> Nil
          | _, lazy (Cons (hd, tl)) -> Lazy.force (drop (n-1) tl)
        )

    let reverse a =
      lazy (
          let rec rev a acc =
            match a with
            | lazy Nil -> acc
            | lazy (Cons (hd, tl)) -> rev tl (Cons (hd, lazy acc))
          in
          rev a Nil
        )
  end

(* Exercise 4.2 - lazy insertion sort *)
open Stream
let rec sort a cmp =
  lazy (
      let rec extract_min a acc acc_min =
        match a with
        | lazy Nil -> acc, acc_min
        | lazy (Cons (hd, tl)) ->
           if cmp hd acc_min < 0 then
             extract_min tl (lazy (Cons (acc_min, acc))) hd
           else
             extract_min tl (lazy (Cons (hd, acc))) acc_min
      in
      match a with
      | lazy Nil -> Nil
      | lazy (Cons (hd, tl)) ->
         let rem, min = extract_min tl (lazy Nil) hd in
         Cons (min, sort rem cmp)
    )
