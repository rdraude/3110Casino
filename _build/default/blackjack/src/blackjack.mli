(** Blackjack is the casino game blackjack, where a bet is placed and based on
    the dealer's cards and your cards, you can either hit or stand. After you
    and the dealer both get a chance to do so, the closest to 21 without going
    above wins the wager*)

type t = { mutable score : int }
(** The type [t] is the abstract type to represent a game of blackjack*)

(** [suit] is the suit of the cards that are used during a round of the game*)
type suit =
  | Spades
  | Diamonds
  | Clubs
  | Hearts

(** [value] is the number on the card or the coart member on the card (i.e.
    Queen)*)
type value =
  | Number of int
  | Ace
  | Jack
  | Queen
  | King

type card = value * suit
(** [card] is a tuple that contains both the value and suit on a card*)

type deck = card list
(** [deck] is a list of cards to mimic a deck of cards*)

type hand = card list
(** [hand] is the list of cards currently in the player's hand*)

val wager_amount : int ref
(** [wager_amount] is the int reference to the amount of money the player is
    betting*)

val game_score : int ref
(** [game_score] refers to the score in the game*)

val get_deck : (value * suit) list -> int -> (value * suit) list
(** [get_deck acc n] returns a deck of playing cards numbered 1-n in each
    playing suite *)

val empty : 'a list
(** [empty] returns an empty list*)

val deal : unit -> 'a list
(** [deal ()] returns an empty list *)

val get_score : int
(** [get_score] returns the initial current score of the game*)

val stand_deck : (value * suit) list
(** [stand_deck] returns a standard deck of cards*)

val int_of_val : value -> int
(** [int_of_val v] returns the number associated with a particular card number v*)

val deck_array_backup : (value * suit) array
(** [deck_array_backuo] returns an array of a standard deck of cards*)

val get_card : unit -> value * suit
(** [get_card()] returns a random card drawn*)

val suit_to_string : suit -> string
(** [suit_to_string s] returns the string representation of the suit s*)

val value_to_string : value -> string
(** [value_to_string v] returns the string representation of the value v*)

val card_to_string : value * suit -> string
(** [card_to_string c] returns the string representation of the card c*)

val create_card_pic : unit -> unit
(** [create_card_pic] prints out a card's value and suit that is drawn randomly*)

val num_aces : deck -> int
(** [num_aces d] returns the number of aces in a deck d*)

val deduct : int -> int -> int (*HELP*)
(** [deduct s a] returns the player's score after deducting the score*)

val calculate_score_of_card : value * 'a -> int
(** [calculate_score_of_card c] returns the number of points associated with a
    card c*)

val calculate_score : (value * 'a) list -> int -> int
(** [calculate_score f acc] returns the number of points given a list of cards f*)

val check_score : deck -> int (*HELP*)
(** [check_score f] returns the number of points given a deck of cards*)

val card_to_pic : value * suit -> string
(** [card_to_pic c] returns a string representation of the card c picked.*)

val deck_to_string : (value * suit) list -> string
(** [decl_to_string d] returns a string representation of deck d*)

val update_your : unit -> unit
(** [update_your ()] prints the player's current hand and score*)

val update_dealer : unit -> unit
(** [update_dealer ()] prints the dealer's current hand and score*)

val game_lost : unit -> unit
(** [game_lost ()] prints both players' scores and hands and will print the
    losing message*)

val game_won : unit -> unit
(** [game_won ()] prints both players' scores and hands and will print the
    winning message*)

val hit_me : unit -> unit
(** [hit_me ()] adds a card to the player's hand and checks if the player has
    one. Returns a printed statement declaring a win or loss *)

val deal_stop : unit -> unit
(** [deal_stop ()] stops the game when the game is no longer running*)

val dealer_hit : unit -> unit
(** [dealer_hit ()] adds a card to the dealer's hand and checks if the dealer
    has won. Returns a printed statmenet declaring a win or a loss*)

val startup : unit -> unit
(** [startup ()] ret*) (*HELP*)

val blackjack_main : unit -> int -> unit
(** [blackjack_main () n] takes in a score n and forms the interactive interface
    of the game for both the player and the dealer*)

val prompt : unit -> unit
(** [prompt ()] will display a prompt to the user about replaying the game or
    not*)

val get_score_bla : unit -> int -> int
(** [game_score_bla () n] will return the score n of the game once the game is
    over*)

val last_input : string ref
(** [last_input] is the last string input, empty string if there are no inputs
    yet*)
