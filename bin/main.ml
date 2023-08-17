open Bjack
open Blackjack
open Roul
open Roulette

(* let rec prompt () = ANSITerminal.print_string [ ANSITerminal.magenta ]
   "\nWould you like to play again : Y/N ? \n"; match read_line () with |
   exception End_of_file -> () | stringin -> if stringin = "Y" then
   blackjack_main () else if stringin = "N" then ( print_string "Exiting
   Blackjack"; exit 0) else print_endline "Invalid command"; prompt () *)

let game_score_main = ref 50

let rec main () =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "\nWelcome to Casino Royale! Type roulette or blackjack! \n";
  print_endline ("Current score: " ^ string_of_int !game_score_main);
  match read_line () with
  | exception End_of_file -> ()
  | stringin ->
      last_input := stringin;
      if !last_input <> "quit" then (
        if !last_input = "roulette" then
          game_score_main := get_score_roul () !game_score_main
        else if !last_input = "blackjack" then
          game_score_main := get_score_bla () !game_score_main
        else ();
        main ())
      else (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nGame Over! Thanks for\n\n   playing! \n";
        exit 0)

let () = main ()
