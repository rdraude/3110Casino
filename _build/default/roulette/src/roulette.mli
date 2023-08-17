(** Roulette is the casino game roulette, where a bet can be placed regarding
    where on the wheel it will land, which either results in winning or losing
    the wager*)

type t = { mutable score : int }
(** The type [t] is the abstract type to represent a game of roulette*)

(** The type [zero] represents the two zero pieces on the roulette wheel *)
type zero =
  | Single
  | Double

(** The type [wheel] represents the different types of spaces on the roulette
    wheel*)
type wheel =
  | Red of int
  | Black of int
  | Green of zero

(** The type [half] represents the different halves of spaces on the roulette
    wheel*)
type half =
  | First
  | Second

(** The type [color] represents the different colors on the roulette wheel*)
type color =
  | Red
  | Black
  | Green

(** The type [twelve] represents the different thirds on the roulette wheel*)
type twelve =
  | One_to_Twelve
  | Thirteen_to_Twentyfour
  | Twentyfive_to_Thirtysix

(** The type [bet] represents the different types of bets the user can place *)
type bet =
  | Color of color
  | Dozen of twelve
  | Half of half
  | Square of wheel

exception InvalidBet of string
(** Raised when an invalid bet is given*)

val start_game : t
(** [start_game] is a new game of abstract type t*)

(*val get_score : t -> int*)

val get_score_roul : unit -> int -> int
(** The score of the game *)

val roul_main : unit -> int -> unit
(** The main function that runs the game*)

val table : wheel array
(** The value [table] is all of the spaces on the roulette wheel *)

val random : unit -> wheel
(** Function that returns a random space on the wheel*)

val number_checker : bet -> int -> wheel -> int -> int
(** The value [number_checker] is the new score based on the result of the game
    and the bet inputted if it is a number bet*)

val color_checker : bet -> int -> wheel -> int -> int
(** The value [color_checker] is the new score based on the result of the game
    and the bet inputted if it is a color bet *)

val dozen_checker : bet -> int -> wheel -> int -> int
(** The value [dozen_checker] is the new score based on the result of the game
    and the bet inputted if it is a dozen bet*)

val calculate_bet : bet -> int -> wheel -> int -> int
(**The value [calculate_bet] is the new score based on the result of the game
   using all types of bets concatenated*)

val run : t -> bet -> int -> t
(** [run] is the new game of abstract type t with a new inputs of bet and wager*)

val res_to_string : wheel -> string
(** Returns [wheel] as a string*)

val match_string_to_fun : string -> bet
(** Returns a bet of the form [string] *)
