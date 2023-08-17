type t = { mutable score : int }
(** The type [t] is the abstract type to represent a game of blackjack*)

type suit =
  | Spades
  | Diamonds
  | Clubs
  | Hearts

(*Number of int must be an integer in the range [2,10]*)
type value =
  | Number of int
  | Ace
  | Jack
  | Queen
  | King

type card = value * suit
type deck = card list
type hand = card list

let wager_amount = ref 0
let game_score = ref 0

(* changed n to 11 so that there would be cards of 10, too recursive base case
   of 10 didnt create 10's*)
let rec get_deck acc n =
  if n = 11 then acc
  else
    let spades = (Number n, Spades) in
    let diamonds = (Number n, Diamonds) in
    let clubs = (Number n, Clubs) in
    let hearts = (Number n, Hearts) in
    get_deck (spades :: diamonds :: clubs :: hearts :: acc) (n + 1)

let empty = []
let deal () = []
let get_score = 0

let stand_deck =
  get_deck empty 2
  @ [
      (Jack, Spades);
      (Jack, Diamonds);
      (Jack, Clubs);
      (Jack, Hearts);
      (Queen, Spades);
      (Queen, Diamonds);
      (Queen, Clubs);
      (Queen, Hearts);
      (King, Spades);
      (King, Diamonds);
      (King, Clubs);
      (King, Hearts);
      (Ace, Spades);
      (Ace, Diamonds);
      (Ace, Clubs);
      (Ace, Hearts);
    ]

let int_of_val (v : value) =
  match v with
  | Number n -> n
  | Ace -> 11
  | _ -> 10

let deck_array_backup =
  [|
    (Number 10, Spades);
    (Number 10, Diamonds);
    (Number 10, Clubs);
    (Number 10, Hearts);
    (Number 9, Spades);
    (Number 9, Diamonds);
    (Number 9, Clubs);
    (Number 9, Hearts);
    (Number 8, Spades);
    (Number 8, Diamonds);
    (Number 8, Clubs);
    (Number 8, Hearts);
    (Number 7, Spades);
    (Number 7, Diamonds);
    (Number 7, Clubs);
    (Number 7, Hearts);
    (Number 6, Spades);
    (Number 6, Diamonds);
    (Number 6, Clubs);
    (Number 6, Hearts);
    (Number 5, Spades);
    (Number 5, Diamonds);
    (Number 5, Clubs);
    (Number 5, Hearts);
    (Number 4, Spades);
    (Number 4, Diamonds);
    (Number 4, Clubs);
    (Number 4, Hearts);
    (Number 3, Spades);
    (Number 3, Diamonds);
    (Number 3, Clubs);
    (Number 3, Hearts);
    (Number 2, Spades);
    (Number 2, Diamonds);
    (Number 2, Clubs);
    (Number 2, Hearts);
    (Jack, Spades);
    (Jack, Diamonds);
    (Jack, Clubs);
    (Jack, Hearts);
    (Queen, Spades);
    (Queen, Diamonds);
    (Queen, Clubs);
    (Queen, Hearts);
    (King, Spades);
    (King, Diamonds);
    (King, Clubs);
    (King, Hearts);
    (Ace, Spades);
    (Ace, Diamonds);
    (Ace, Clubs);
    (Ace, Hearts);
  |]

let deck_array = Array.of_list stand_deck

let get_card () =
  Random.self_init ();
  deck_array.(Random.int 51)

let suit_to_string suit =
  match suit with
  | Hearts -> "Hearts"
  | Clubs -> "Clubs"
  | Diamonds -> "Diamonds"
  | Spades -> "Spades"

let value_to_string value =
  match value with
  | Queen -> "Queen"
  | Jack -> "Jack"
  | King -> "King"
  | Ace -> "Ace"
  | Number a -> string_of_int a

let card_to_string card =
  match card with
  | Number 10, Spades -> "10 of Spades"
  | Number 10, Diamonds -> "10 of Diamonds"
  | Number 10, Clubs -> "10 of Clubs"
  | Number 10, Hearts -> "10 of Hearts"
  | Number 9, Spades -> "9 of Spades"
  | Number 9, Diamonds -> "9 of Diamonds"
  | Number 9, Clubs -> "9 of Clubs"
  | Number 9, Hearts -> "9 of Hearts"
  | Number 8, Spades -> "8 of Spades"
  | Number 8, Diamonds -> "8 of Diamonds"
  | Number 8, Clubs -> "8 of Clubs"
  | Number 8, Hearts -> "8 of Hearts"
  | Number 7, Spades -> "7 of Spades"
  | Number 7, Diamonds -> "7 of Diamonds"
  | Number 7, Clubs -> "7 of Clubs"
  | Number 7, Hearts -> "7 of Hearts"
  | Number 6, Spades -> "6 of Spades"
  | Number 6, Diamonds -> "6 of Diamonds"
  | Number 6, Clubs -> "6 of Clubs"
  | Number 6, Hearts -> "6 of Hearts"
  | Number 5, Spades -> "5 of Spades"
  | Number 5, Diamonds -> "5 of Diamonds"
  | Number 5, Clubs -> "5 of Clubs"
  | Number 5, Hearts -> "5 of Hearts"
  | Number 4, Spades -> "4 of Spades"
  | Number 4, Diamonds -> "4 of Diamonds"
  | Number 4, Clubs -> "4 of Clubs"
  | Number 4, Hearts -> "4 of Hearts"
  | Number 3, Spades -> "3 of Spades"
  | Number 3, Diamonds -> "3 of Diamonds"
  | Number 3, Clubs -> "3 of Clubs"
  | Number 3, Hearts -> "3 of Hearts"
  | Number 2, Spades -> "2 of Spades"
  | Number 2, Diamonds -> "2 of Diamonds"
  | Number 2, Clubs -> "2 of Clubs"
  | Number 2, Hearts -> "2 of Hearts"
  | Jack, Spades -> "Jack of Spades"
  | Jack, Diamonds -> "Jack of Diamonds"
  | Jack, Clubs -> "Jack of Clubs"
  | Jack, Hearts -> "Jack of Hearts"
  | Queen, Spades -> "Queen of Spades"
  | Queen, Diamonds -> "Queen of Diamonds"
  | Queen, Clubs -> "Queen of Clubs"
  | Queen, Hearts -> "Queen of Hearts"
  | King, Spades -> "King of Spades"
  | King, Diamonds -> "King of Diamonds"
  | King, Clubs -> "King of Clubs"
  | King, Hearts -> "King of Hearts"
  | Ace, Spades -> "Ace of Spades"
  | Ace, Diamonds -> "Ace of Diamonds"
  | Ace, Clubs -> "Ace of Clubs"
  | Ace, Hearts -> "Ace of Hearts"
  | Number _, _ -> failwith "not a card!"

let create_card_pic () =
  match get_card () with
  | a, suit ->
      print_endline
        ("Card: " ^ value_to_string a ^ " of suit " ^ suit_to_string suit)

(* need a function to check if you are over and have an ace -> ace goes to
   one *)
let rec num_aces (d : deck) =
  match d with
  | [] -> 0
  | (v, s) :: t -> if v = Ace then 1 + num_aces t else num_aces t

let deduct score aces =
  let num = ref aces in
  let score_in = ref score in
  while !score_in > 21 && !num > 0 do
    score_in := !score_in - 10;
    num := !num - 1
  done;
  !score_in

(* let calculate_score_ace_one card = match card with | Number a, suit -> a |
   face, suit -> if face = Ace then 1 else 10 *)

let calculate_score_of_card card =
  match card with
  | Number a, suit -> a
  | face, suit -> if face = Ace then 11 else 10

let rec calculate_score flop acc =
  match flop with
  | [] -> acc
  | h :: t -> calculate_score_of_card h + calculate_score t acc

(* let rec calculate_score_over flop acc = match flop with | [] -> acc | h :: t
   -> calculate_score_ace_one h + calculate_score_over t acc *)

let check_score flop =
  if calculate_score flop 0 <= 21 then calculate_score flop 0
  else deduct (calculate_score flop 0) (num_aces flop)

let ace_of_hearts = "   ___\n  |A  |\n  | <3|\n  |__A|"
let two_of_hearts = "   ___\n  |2  |\n  | <3|\n  |__2|"
let three_of_hearts = "   ___\n  |3  |\n  | <3|\n  |__3|"
let four_of_hearts = "   ___\n  |4  |\n  | <3|\n  |__4|"
let five_of_hearts = "   ___\n  |5  |\n  | <3|\n  |__5|"
let six_of_hearts = "   ___\n  |6  |\n  | <3|\n  |__6|"
let seven_of_hearts = "   ___\n  |7  |\n  | <3|\n  |__7|"
let eight_of_hearts = "   ___\n  |8  |\n  | <3|\n  |__8|"
let nine_of_hearts = "   ___\n  |9  |\n  | <3|\n  |__9|"
let ten_of_hearts = "   ___\n  |10 |\n  | <3|\n  |_10|"
let jack_of_hearts = "   ___\n  |J  |\n  | <3|\n  |__J|"
let queen_of_hearts = "   ___\n  |Q  |\n  | <3|\n  |__Q|"
let king_of_hearts = "   ___\n  |K  |\n  | <3|\n  |__K|"
let ace_of_spades = "   ___\n  |A  |\n  | -D|\n  |__A|"
let two_of_spades = "   ___\n  |2  |\n  | -D|\n  |__2|"
let three_of_spades = "   ___\n  |3  |\n  | -D|\n  |__3|"
let four_of_spades = "   ___\n  |4  |\n  | -D|\n  |__4|"
let five_of_spades = "   ___\n  |5  |\n  | -D|\n  |__5|"
let six_of_spades = "   ___\n  |6  |\n  | -D|\n  |__6|"
let seven_of_spades = "   ___\n  |7  |\n  | -D|\n  |__7|"
let eight_of_spades = "   ___\n  |8  |\n  | -D|\n  |__8|"
let nine_of_spades = "   ___\n  |9  |\n  | -D|\n  |__9|"
let ten_of_spades = "   ___\n  |10 |\n  | -D|\n  |_10|"
let jack_of_spades = "   ___\n  |J  |\n  | -D|\n  |__J|"
let queen_of_spades = "   ___\n  |Q  |\n  | -D|\n  |__Q|"
let king_of_spades = "   ___\n  |K  |\n  | -D|\n  |__K|"
let ace_of_diamonds = "   ___\n  |A  |\n  | <>|\n  |__A|"
let two_of_diamonds = "   ___\n  |2  |\n  | <>|\n  |__2|"
let three_of_diamonds = "   ___\n  |3  |\n  | <>|\n  |__3|"
let four_of_diamonds = "   ___\n  |4  |\n  | <>|\n  |__4|"
let five_of_diamonds = "   ___\n  |5  |\n  | <>|\n  |__5|"
let six_of_diamonds = "   ___\n  |6  |\n  | <>|\n  |__6|"
let seven_of_diamonds = "   ___\n  |7  |\n  | <>|\n  |__7|"
let eight_of_diamonds = "   ___\n  |8  |\n  | <>|\n  |__8|"
let nine_of_diamonds = "   ___\n  |9  |\n  | <>|\n  |__9|"
let ten_of_diamonds = "   ___\n  |10 |\n  | <>|\n  |_10|"
let jack_of_diamonds = "   ___\n  |J  |\n  | <>|\n  |__J|"
let queen_of_diamonds = "   ___\n  |Q  |\n  | <>|\n  |__Q|"
let king_of_diamonds = "   ___\n  |K  |\n  | <>|\n  |__K|"
let ace_of_clubs = "   ___\n  |A  |\n  | -@|\n  |__A|"
let two_of_clubs = "   ___\n  |2  |\n  | -@|\n  |__2|"
let three_of_clubs = "   ___\n  |3  |\n  | -@|\n  |__3|"
let four_of_clubs = "   ___\n  |4  |\n  | -@|\n  |__4|"
let five_of_clubs = "   ___\n  |5  |\n  | -@|\n  |__5|"
let six_of_clubs = "   ___\n  |6  |\n  | -@|\n  |__6|"
let seven_of_clubs = "   ___\n  |7  |\n  | -@|\n  |__7|"
let eight_of_clubs = "   ___\n  |8  |\n  | -@|\n  |__8|"
let nine_of_clubs = "   ___\n  |9  |\n  | -@|\n  |__9|"
let ten_of_clubs = "   ___\n  |10 |\n  | -@|\n  |_10|"
let jack_of_clubs = "   ___\n  |J  |\n  | -@|\n  |__J|"
let queen_of_clubs = "   ___\n  |Q  |\n  | -@|\n  |__Q|"
let king_of_clubs = "   ___\n  |K  |\n  | -@|\n  |__K|"

let card_to_pic card =
  match card with
  | Number 10, Spades -> ten_of_spades
  | Number 10, Diamonds -> ten_of_diamonds
  | Number 10, Clubs -> ten_of_clubs
  | Number 10, Hearts -> ten_of_hearts
  | Number 9, Spades -> nine_of_spades
  | Number 9, Diamonds -> nine_of_diamonds
  | Number 9, Clubs -> nine_of_clubs
  | Number 9, Hearts -> nine_of_hearts
  | Number 8, Spades -> eight_of_spades
  | Number 8, Diamonds -> eight_of_diamonds
  | Number 8, Clubs -> eight_of_clubs
  | Number 8, Hearts -> eight_of_hearts
  | Number 7, Spades -> seven_of_spades
  | Number 7, Hearts -> seven_of_hearts
  | Number 7, Diamonds -> seven_of_diamonds
  | Number 7, Clubs -> seven_of_clubs
  | Number 6, Spades -> six_of_spades
  | Number 6, Diamonds -> six_of_diamonds
  | Number 6, Clubs -> six_of_clubs
  | Number 6, Hearts -> six_of_hearts
  | Number 5, Spades -> five_of_spades
  | Number 5, Diamonds -> five_of_diamonds
  | Number 5, Clubs -> five_of_clubs
  | Number 5, Hearts -> five_of_hearts
  | Number 4, Spades -> four_of_spades
  | Number 4, Diamonds -> four_of_diamonds
  | Number 4, Clubs -> four_of_clubs
  | Number 4, Hearts -> four_of_hearts
  | Number 3, Spades -> three_of_spades
  | Number 3, Diamonds -> three_of_diamonds
  | Number 3, Clubs -> three_of_clubs
  | Number 3, Hearts -> three_of_hearts
  | Number 2, Spades -> two_of_spades
  | Number 2, Diamonds -> two_of_diamonds
  | Number 2, Clubs -> two_of_clubs
  | Number 2, Hearts -> two_of_hearts
  | Jack, Spades -> jack_of_spades
  | Jack, Diamonds -> jack_of_diamonds
  | Jack, Clubs -> jack_of_clubs
  | Jack, Hearts -> jack_of_hearts
  | Queen, Spades -> queen_of_spades
  | Queen, Diamonds -> queen_of_diamonds
  | Queen, Clubs -> queen_of_clubs
  | Queen, Hearts -> queen_of_hearts
  | King, Spades -> king_of_spades
  | King, Diamonds -> king_of_diamonds
  | King, Clubs -> king_of_clubs
  | King, Hearts -> king_of_hearts
  | Ace, Spades -> ace_of_spades
  | Ace, Diamonds -> ace_of_diamonds
  | Ace, Clubs -> ace_of_clubs
  | Ace, Hearts -> ace_of_hearts
  | Number _, _ -> failwith "not a card!"

let game_is_running = ref true
let (empty_hand : deck) = []
let (your_hand : deck ref) = ref empty_hand
let (dealer_hand : deck ref) = ref empty_hand

let rec deck_to_string de =
  match de with
  | [] -> ""
  | h :: t -> card_to_string h ^ "\n " ^ card_to_pic h ^ "\n" ^ deck_to_string t

let start_game = { score = 0 }

let update_your () =
  print_endline ("Your Hand: \n" ^ deck_to_string !your_hand);
  print_endline (string_of_int (check_score !your_hand))

let update_dealer () =
  print_endline ("Dealer's Hand: \n" ^ deck_to_string !dealer_hand);
  print_endline (string_of_int (check_score !dealer_hand))

let game_lost () : unit =
  print_endline ("Dealer's Hand: \n" ^ deck_to_string !dealer_hand);
  print_endline (string_of_int (check_score !dealer_hand));
  print_endline ("Your Hand: \n" ^ deck_to_string !your_hand);
  print_endline (string_of_int (check_score !your_hand));
  print_endline "Sorry, you lost to the house";
  game_score := !game_score - !wager_amount;
  game_is_running := false

let game_won () : unit =
  print_endline ("Dealer's Hand: \n" ^ deck_to_string !dealer_hand);
  print_endline (string_of_int (check_score !dealer_hand));
  print_endline ("Your Hand: \n" ^ deck_to_string !your_hand);
  print_endline (string_of_int (check_score !your_hand));
  print_endline "Congratulations, your hand beat the dealer's";
  game_score := !game_score + (3 * !wager_amount / 2);
  game_is_running := false

let hit_me () =
  let new_card = get_card () in
  your_hand := new_card :: !your_hand;
  if check_score !your_hand > 21 then game_lost () else ()

let deal_stop () : unit = game_is_running := false

let rec dealer_hit () =
  let d = check_score !dealer_hand in
  if check_score !your_hand > d then dealer_hand := get_card () :: !dealer_hand;
  if check_score !dealer_hand > 21 then game_won ()
  else if check_score !dealer_hand = 21 then game_lost ()
  else if check_score !dealer_hand >= check_score !your_hand then game_lost ()
  else if d > 17 then ()
  else dealer_hit ()

let last_input = ref ""

let startup () =
  let first_card = get_card () in
  let second_card = get_card () in
  your_hand := [ first_card; second_card ];
  let dealer_first = get_card () in
  let dealer_second = get_card () in
  dealer_hand := [ dealer_first; dealer_second ]

let rec blackjack_main () score =
  game_score := score;
  print_endline (string_of_int !game_score);
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWelcome to Casino Royale! Current game: Blackjack! \n";
  startup ();
  (*our gameplay*)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nType in a number for your wager:\n";
  match read_line () with
  | exception End_of_file -> ()
  | stringin ->
      wager_amount := int_of_string stringin;
      startup ();
      while !game_is_running do
        update_dealer ();
        update_your ();
        print_endline "Type hit or stand";
        match read_line () with
        | exception End_of_file -> ()
        | stringin ->
            last_input := stringin;
            if !last_input = "hit" then hit_me ()
            else if !last_input = "stand" then dealer_hit ()
            else game_is_running := false
      done;

      game_is_running := true;
      prompt ()

and prompt () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("\nCurrent Score: " ^ string_of_int !game_score);
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\nWould you like to play again : Y/N ? \n";
  match read_line () with
  | exception End_of_file -> ()
  | stringin ->
      if stringin = "Y" then blackjack_main () !game_score
      else if stringin = "N" then print_string "Exiting Blackjack"
      else (
        print_endline "Invalid command";
        prompt ())

let get_score_bla () score_from_main =
  blackjack_main () score_from_main;
  !game_score

(* the following is the overall main function to switch between roulette and
   blackjack *)

(* let aux_is_running = ref 10

   let aux () = while !aux_is_running > 0 do print_endline "aux"; aux_is_running
   := !aux_is_running - 1 done; print_string "done"

   let aux2_is_running = ref 10

   let aux2 () = while !aux2_is_running > 0 do print_endline "aux222";
   aux2_is_running := !aux2_is_running - 1 done; print_string "done"

   let last_input = ref "" *)

(* let rec main () = ANSITerminal.print_string [ ANSITerminal.yellow ]
   "\nWelcome to Casino Royale! Type roulette or blackjack! \n"; match read_line
   () with | exception End_of_file -> () | stringin -> last_input := stringin;
   if !last_input <> "quit" then ( if !last_input = "roulette" then aux () else
   if !last_input = "blackjack" then blackjack_main () else (); main ()) else
   (); ANSITerminal.print_string [ ANSITerminal.red ] "\nGame Over! Thanks for\n
   playing! \n" *)
