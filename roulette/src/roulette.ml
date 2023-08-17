(** The type [zero] is used to represent the 0 or 00 green spaces on the wheel*)
type zero =
  | Single
  | Double

type wheel =
  | Red of int
  | Black of int
  | Green of zero

type t = { mutable score : int }

let start_game = { score = 0 }

exception InvalidBet of string

let score_of_roulette = ref 0

let table =
  [|
    Green Single;
    Red 1;
    Black 2;
    Red 3;
    Black 4;
    Red 5;
    Black 6;
    Red 7;
    Black 8;
    Red 9;
    Black 10;
    Black 11;
    Red 12;
    Black 13;
    Red 14;
    Black 15;
    Red 16;
    Black 17;
    Red 18;
    Red 19;
    Black 20;
    Red 21;
    Black 22;
    Red 23;
    Black 24;
    Red 25;
    Black 26;
    Red 27;
    Black 28;
    Black 29;
    Red 30;
    Black 31;
    Red 32;
    Black 33;
    Red 34;
    Black 35;
    Red 36;
    Green Double;
  |]

(** The type [color] is used to represent the color of the space on the wheel*)
type color =
  | Red
  | Black
  | Green

(** The type [twelve] is used to represent the divisions by three of the wheel*)
type twelve =
  | One_to_Twelve
  | Thirteen_to_Twentyfour
  | Twentyfive_to_Thirtysix

(** The type [half] is used to represent the divisions by two of the wheel*)
type half =
  | First
  | Second

type bet =
  | Color of color
  | Dozen of twelve
  | Half of half
  | Square of wheel

let bets =
  [|
    Square (Green Single);
    Square (Red 1);
    Square (Black 2);
    Square (Red 3);
    Square (Black 4);
    Square (Red 5);
    Square (Black 6);
    Square (Red 7);
    Square (Black 8);
    Square (Red 9);
    Square (Black 10);
    Square (Black 11);
    Square (Red 12);
    Square (Black 13);
    Square (Red 14);
    Square (Black 15);
    Square (Red 16);
    Square (Black 17);
    Square (Red 18);
    Square (Red 19);
    Square (Black 20);
    Square (Red 21);
    Square (Black 22);
    Square (Red 23);
    Square (Black 24);
    Square (Red 25);
    Square (Black 26);
    Square (Red 27);
    Square (Black 28);
    Square (Black 29);
    Square (Red 30);
    Square (Black 31);
    Square (Red 32);
    Square (Black 33);
    Square (Red 34);
    Square (Black 35);
    Square (Red 36);
    Square (Green Double);
    Color Red;
    Color Black;
    Color Green;
    Dozen One_to_Twelve;
    Dozen Thirteen_to_Twentyfour;
    Dozen Twentyfive_to_Thirtysix;
    Half First;
    Half Second;
  |]

let double_zero = "________\n |Green |\n  | 00 |  \n |__|"
let single_zero = "________\n |Green |\n  |  0 |\n   |__|"
let red_one = "________\n |Red   |\n  |  1 |\n   |__|"
let red_three = "________\n |Red   |\n  |  3 |\n   |__|"
let red_five = "________\n |Red   |\n  |  5 |\n   |__|"
let red_seven = "________\n |Red   |\n  | 7  |\n   |__|"
let red_nine = "________\n |Red   |\n  | 9  |\n   |__|"
let red_twelve = "________\n |Red   |\n  | 12 |\n   |__|"
let red_fourteen = "________\n |Red   |\n  | 14 |\n   |__|"
let red_sixteen = "________\n |Red   |\n  | 16 |\n   |__|"
let red_eighteen = "________\n |Red   |\n  | 18 |\n   |__|"
let red_nineteen = "________\n |Red   |\n  | 19 |\n   |__|"
let red_twentyone = "________\n |Red   |\n  | 21 |\n   |__|"
let red_twentythree = "________\n |Red   |\n  | 23 |\n   |__|"
let red_twentyfive = "________\n |Red   |\n  | 25 |\n   |__|"
let red_twentyseven = "________\n |Red   |\n  | 27 |\n   |__|"
let red_thirty = "________\n |Red   |\n  | 30 |\n   |__|"
let red_thirtytwo = "________\n |Red   |\n  | 32 |\n   |__|"
let red_thirtyfour = "________\n |Red   |\n  | 34 |\n   |__|"
let red_thirtysix = "________\n |Red   |\n  | 36 |\n   |__|"
let black_two = "________\n |Black |\n  |  2 |\n   |__|"
let black_four = "________\n |Black |\n  |  4 |\n   |__|"
let black_six = "________\n |Black |\n  |  6 |\n   |__|"
let black_eight = "________\n |Black |\n  |  8 |\n   |__|"
let black_ten = "________\n |Black |\n  | 10 |\n   |__|"
let black_eleven = "________\n |Black |\n  | 11 |\n   |__|"
let black_thirteen = "________\n |Black |\n  | 13 |\n   |__|"
let black_fifteen = "________\n |Black |\n  | 15 |\n   |__|"
let black_seventeen = "________\n |Black |\n  | 17 |\n   |__|"
let black_twenty = "________\n |Black |\n  | 20 |\n   |__|"
let black_twentytwo = "________\n |Black |\n  | 22 |\n   |__|"
let black_twentyfour = "________\n |Black |\n  | 24 |\n   |__|"
let black_twentysix = "________\n |Black |\n  | 26 |\n   |__|"
let black_twentyeight = "________\n |Black |\n  | 28 |\n   |__|"
let black_twentynine = "________\n |Black |\n  | 29 |\n   |__|"
let black_thirtyone = "________\n |Black |\n  | 31 |\n   |__|"
let black_thirtythree = "________\n |Black |\n  | 33 |\n   |__|"
let black_thirtyfive = "________\n |Black |\n  | 35 |\n   |__|"

let res_to_string (res : wheel) =
  match res with
  | Green Single -> single_zero
  | Green Double -> double_zero
  | Red 1 -> red_one
  | Black 2 -> black_two
  | Red 3 -> red_three
  | Black 4 -> black_four
  | Red 5 -> red_five
  | Black 6 -> black_six
  | Red 7 -> red_seven
  | Black 8 -> black_eight
  | Red 9 -> red_nine
  | Black 10 -> black_ten
  | Black 11 -> black_eleven
  | Red 12 -> red_twelve
  | Black 13 -> black_thirteen
  | Red 14 -> red_fourteen
  | Black 15 -> black_fifteen
  | Red 16 -> red_sixteen
  | Black 17 -> black_seventeen
  | Red 18 -> red_eighteen
  | Red 19 -> red_nineteen
  | Black 20 -> black_twenty
  | Red 21 -> red_twentyone
  | Black 22 -> black_twentytwo
  | Red 23 -> red_twentythree
  | Black 24 -> black_twentyfour
  | Red 25 -> red_twentyfive
  | Black 26 -> black_twentysix
  | Red 27 -> red_twentyseven
  | Black 28 -> black_twentyeight
  | Black 29 -> black_twentynine
  | Red 30 -> red_thirty
  | Black 31 -> black_thirtyone
  | Red 32 -> red_thirtytwo
  | Black 33 -> black_thirtythree
  | Red 34 -> red_thirtyfour
  | Black 35 -> black_thirtyfive
  | Red 36 -> red_thirtysix
  | Red _ -> failwith "not an option"
  | Black _ -> failwith "not an option"

let result_print (res : wheel) = print_endline (res_to_string res)

let number_checker bet_in (wager : int) (result : wheel) (score : int) =
  match bet_in with
  | Square whe -> if result = whe then score + (35 * wager) else score - wager
  | _ -> score

let color_checker bet_in (wager : int) (result : wheel) (score : int) =
  match bet_in with
  | Color c -> (
      match result with
      | Green _ -> if c = Green then score + (18 * wager) else score - wager
      | Red _ -> if c = Red then score + wager else score - wager
      | Black _ -> if c = Black then score + wager else score - wager)
  | _ -> score

let dozen_helper (result : wheel) d score wager =
  match result with
  | Green _ -> score - wager
  | Red i ->
      if
        (d = One_to_Twelve && 1 <= i && 12 >= i)
        || (d = Thirteen_to_Twentyfour && 13 <= i && 24 >= i)
        || (d = Twentyfive_to_Thirtysix && 25 <= i && 36 >= i)
      then score + (2 * wager)
      else score - wager
  | Black i ->
      if
        (d = One_to_Twelve && 1 <= i && 12 >= i)
        || (d = Thirteen_to_Twentyfour && 13 <= i && 24 >= i)
        || (d = Twentyfive_to_Thirtysix && 25 <= i && 36 >= i)
      then score + (2 * wager)
      else score - wager

let dozen_checker bet_in (wager : int) (result : wheel) (score : int) =
  match bet_in with
  | Dozen d -> dozen_helper result d score wager
  | _ -> score

(** [random] is a random wheel from [table]*)
let random () =
  Random.self_init ();
  table.(Random.int 37)

(*function to update result ref variable for result*)

let result = ref table.(0)

let update_result =
  result := random ();
  !result

let calculate_bet bet_in (wager : int) (result : wheel) (score : int) =
  number_checker bet_in wager result score
  |> color_checker bet_in wager result
  |> dozen_checker bet_in wager result

let run (gm : t) bet_in (wager : int) =
  match wager with
  | x when 0 <= x && x <= gm.score ->
      gm.score <- calculate_bet bet_in wager update_result gm.score;
      gm
  | _ -> raise (InvalidBet ("You have " ^ string_of_int gm.score ^ " points"))

(* type bet_phrase = string list *)

(* type command = | Bet of bet_phrase | Quit *)

exception Empty
exception Malformed of string

let ( !=! ) x = String.length x > 0

let match_string_to_fun s =
  if s = "Green 0" then Square (Green Single)
  else if s = "Red 1" then Square (Red 1)
  else if s = "Black 2" then Square (Black 2)
  else if s = "Red 3" then Square (Red 3)
  else if s = "Black 4" then Square (Black 4)
  else if s = "Red 5" then Square (Red 5)
  else if s = "Black 6" then Square (Black 6)
  else if s = "Red 7" then Square (Red 7)
  else if s = "Black 8" then Square (Black 8)
  else if s = "Red 9" then Square (Red 9)
  else if s = "Black 10" then Square (Black 10)
  else if s = "Black 11" then Square (Black 11)
  else if s = "Red 12" then Square (Red 12)
  else if s = "Black 13" then Square (Black 13)
  else if s = "Red 14" then Square (Red 14)
  else if s = "Black 15" then Square (Black 15)
  else if s = "Red 16" then Square (Red 16)
  else if s = "Black 17" then Square (Black 17)
  else if s = "Red 18" then Square (Red 18)
  else if s = "Red 19" then Square (Red 19)
  else if s = "Black 20" then Square (Black 20)
  else if s = "Red 21" then Square (Red 21)
  else if s = "Black 22" then Square (Black 22)
  else if s = "Red 23" then Square (Red 23)
  else if s = "Black 24" then Square (Black 24)
  else if s = "Red 25" then Square (Red 25)
  else if s = "Black 26" then Square (Black 26)
  else if s = "Red 27" then Square (Red 27)
  else if s = "Black 28" then Square (Black 28)
  else if s = "Black 29" then Square (Black 29)
  else if s = "Red 30" then Square (Red 30)
  else if s = "Black 31" then Square (Black 31)
  else if s = "Red 32" then Square (Red 32)
  else if s = "Black 33" then Square (Black 33)
  else if s = "Red 34" then Square (Red 34)
  else if s = "Black 35" then Square (Black 35)
  else if s = "Red 36" then Square (Red 36)
  else if s = "Green Double" then Square (Green Double)
  else if s = "Color Red" then Color Red
  else if s = "Color Black" then Color Black
  else if s = "Color Green" then Color Green
  else if s = "Dozen One_to_Twelve" then Dozen One_to_Twelve
  else if s = "Dozen Thirteen_to_Twentyfour" then Dozen Thirteen_to_Twentyfour
  else if s = "Dozen Twentyfive_to_Thirtysix" then Dozen Twentyfive_to_Thirtysix
  else if s = "Half First" then Half First
  else if s = "Half Second" then Half Second
  else raise (InvalidBet "Not a bet")

let parse str bet =
  str |> String.split_on_char ' ' |> List.filter ( !=! ) |> function
  | h :: t ->
      if h = "quit" then ()
      else if h = "bet" && t <> [] then (
        result := random ();
        result_print !result;
        score_of_roulette :=
          calculate_bet
            (match_string_to_fun (String.concat " " t))
            bet !result !score_of_roulette)
      else raise (Malformed h)
  | [] -> raise Empty

(* and wrong_input = print_endline "" *)
(* TODO:here we should have a safeguard function against wrong inputs, and
   another function that will display "help" menu...list of all valid bets *)

let play str bet =
  try
    parse str bet;
    print_endline (string_of_int !score_of_roulette)
  with Sys_error str -> print_endline str

let game_is_running = ref true

let roul_main () score_in_from_main =
  score_of_roulette := score_in_from_main;

  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWelcome to Casino Royale! Current game: roulette! \n";
  while !game_is_running do
    print_endline
      "Please enter the amount you want to bet, then the name of your bet. \n";
    match read_line () with
    | exception End_of_file -> ()
    | stringin ->
        (if stringin = "quit" then game_is_running := false
        else
          stringin |> String.split_on_char ' ' |> List.filter ( !=! )
          |> function
          | h :: t ->
              if int_of_string h > 0 && int_of_string h <= !score_of_roulette
              then play ("bet " ^ String.concat " " t) (int_of_string h)
              else print_endline "Invalid bet"
          | _ -> ());
        if !score_of_roulette <= 0 then game_is_running := false
  done;
  print_string "Game over"

let get_score_roul () score_from_main =
  roul_main () score_from_main;
  game_is_running := true;
  !score_of_roulette

(* let () = main () *)
