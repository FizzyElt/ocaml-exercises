(* https://cs3110.github.io/textbook/chapters/basics/exercises.html *)

(* values *)
let result = 7 * (1 + 2 + 3)
let result2 = "CS " ^ string_of_format "3110"

(* operators *)
let operator1 = 42 * 10
let operator2 = 3.14 /. 2.0
let operator3 = 4.2 ** 7.0

(* equality *)

(* structural equality *)
let equality1 = 42 = 42

(* structural equality *)
let equality2 = "hi" = "hi"

(* physical equality *)
let equality3 = "hi" == "hi"

(* assert *)
let assert1 = assert true
let assert2 = assert false
let assert3 = assert (2110 = 3110)

(* if *)
let ifelse = if 2 > 1 then 42 else 7

(* double fun *)
let double x = x * 2

(* more fun *)
let cube n = n ** 3.0
let sign n = if n > 0 then 1 else if n < 0 then -1 else 0
let area_of_circle r = (r ** 2.0) *. 3.14

(* RMS *)
let rms x y = sqrt (((x *. x) +. (y *. y)) /. 2.0)

(* date fun *)
let is_valid_date d m =
  match m with
  | "Feb" -> 1 <= d && d <= 28
  | "Apr" | "Jun" | "Sep" | "Nov" -> 1 <= d && d <= 30
  | "Jan" | "Mar" | "May" | "Jul" | "Aug" | "Oct" | "Dec" -> 1 <= d && d <= 31
  | _ -> false

(* fib *)
let rec fib n =
  if n = 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)

(* fib fast *)
let fib_fast n =
  let rec h n pp p = if n = 0 then p else h (n - 1) p (pp + p) in
  h n 0 1

(* poly types *)

(* bool -> bool *)
let f x = if x then x else x

(* 'a -> bool -> 'a *)
let g x y = if y then x else x

(* bool -> 'a -> 'a -> 'a *)
let h x y z = if x then y else z

(* bool -> 'a -> 'b -> 'a *)
let i x y _z = if x then y else y

(* divide *)
let divide numerator denominator = numerator /. denominator

(* associativity *)
let add x y = x + y

(* add 5 1 = 6 *)
(* add 5 = fun x -> x + 5 *)
(* (add 5) 1 = 6 *)
(* add (1 5)  error *)

(* average *)
let ( +/. ) x y = (x +. y) /. 2.0
