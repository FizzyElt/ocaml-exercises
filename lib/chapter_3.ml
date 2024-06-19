(* https://cs3110.github.io/textbook/chapters/data/exercises.html *)

(* patterns *)

let pattern_match list =
  match list with
  | e :: _ when e = "bigred" -> true
  | [ _; _ ] | [ _; _; _; _ ] -> true
  | e1 :: e2 :: _ when e1 = e2 -> true
  | _ -> false

(* library *)

(* fifth element *)
let fifth_ele list = List.nth_opt list 5 |> Option.value ~default:0

(* sort *)
let sort_num list = List.sort Stdlib.compare list |> List.rev

(* library puzzle *)
let last list = List.length list - 1 |> List.nth list

(* any zero *)
let any_zero list = List.exists (Int.equal 0) list

let rec take n list =
  match (n, list) with
  | 0, _ -> []
  | _, [] -> []
  | _, h :: t -> h :: take (n - 1) t

let rec drop n list =
  match (n, list) with
  | 0, list -> list
  | _, _ :: tail -> drop (n - 1) tail
  | _, [] -> []

let take_tail_rec n list =
  let rec help res n list =
    match (n, list) with
    | 0, _ -> List.rev res
    | _, [] -> List.rev res
    | _, h :: t -> help (h :: res) (n - 1) t
  in
  help [] n list

let is_unimodal lst =
  let rec increasing = function
    | [] | [ _ ] -> true
    | x :: y :: rest ->
        if x <= y then increasing (y :: rest) else decreasing (y :: rest)
  and decreasing = function
    | [] | [ _ ] -> true
    | x :: y :: rest -> x >= y && decreasing (y :: rest)
  in
  increasing lst

let powerset lst =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (acc @ List.map (fun subset -> x :: subset) acc) xs
  in
  aux [ [] ] lst

type student = { first_name : string; last_name : string; gpa : float }

let s = { first_name = "John"; last_name = "Doe"; gpa = 4.0 }
let s_name s = (s.first_name, s.last_name)
let create_s first_name last_name gpa = { first_name; last_name; gpa }

type poketype = Normal | Fire | Water
type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = { name = "Charizard"; hp = 78; ptype = Fire }
let squirtle = { name = "Squirtle"; hp = 44; ptype = Water }

(* safe hd and tl *)
let safe_hd list = match list with e :: _ -> Some e | [] -> None
let safe_tl list = match list with [] -> None | _ :: t -> Some t

(* pokefun *)
let max_hp (list : pokemon list) =
  List.fold_left
    (fun acc e -> acc |> Option.map (fun x -> if e.hp > x.hp then e else x))
    None list

type datelike = int * int * int

let is_before (d1 : datelike) (d2 : datelike) =
  let y1, m1, d1 = d1 in
  let y2, m2, d2 = d2 in
  y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)

let earliest date_list =
  List.fold_left
    (fun acc e -> acc |> Option.map (fun x -> if is_before e x then e else x))
    None date_list

type suit = Heart | Diamond | Club | Spade

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type card = { suit : suit; rank : rank }

let club_ace = { suit = Club; rank = Ace }
let heart_queen = { suit = Heart; rank = Queen }
let diamond_two = { suit = Diamond; rank = Two }
let spade_seven = { suit = Spade; rank = Seven }

(* quadrant *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x : int) : sign = if x = 0 then Zero else if x < 0 then Neg else Pos

let quadrant : int * int -> quad option =
 fun (x, y) ->
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _ -> None
