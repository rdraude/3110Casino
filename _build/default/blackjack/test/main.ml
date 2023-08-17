open OUnit2
(** Test Plan:

    The functions that ran the key logic behind the game were tested by OUnit in
    this main file. The other functions that provided the interactive interface
    with the player (and had output unit) were tested manually by manually
    playing the game.

    For this testing file, the Blackjack module was tested by OUnit.
    Additionally, these test cases were developed by both glass box and black
    box testing. For black box testing, the implementer solely looked at the mli
    descriptions of the functions as inspiration for the test cases that needed
    be run. For glass box testing, the implementor looked at the functions first
    extensively to understand how they worked, and then developed test cases
    that targeted boundary cases and other key components of the function.

    This testing approach demonstrates the correctness of the system due to the
    extensiveness of the tests. The majority of the functions were run on an
    entire deck of cards to eliminate any inconsistencies and to ensure that the
    logic behind calculating the score was done correctly. Debugging the code in
    this way was very helpful, as numerous small errors were identified and
    subsequently resolved as a result. *)

open Bjack
open Blackjack

let test_card1 = (Ace, Diamonds)
let test_card2 = (Queen, Hearts)
let test_card3 = (Number 8, Clubs)
let test_card4 = (Jack, Spades)
let test_card5 = (Number 10, Diamonds)
let test_card6 = (Number 2, Hearts)
let test_card7 = (Number 3, Clubs)
let test_card8 = (Number 4, Spades)
let test_card9 = (Number 5, Diamonds)
let test_card10 = (Number 6, Hearts)
let test_card11 = (Number 7, Clubs)
let test_card12 = (Number 9, Spades)
let test_card13 = (King, Diamonds)
let test_card14 = (Number 2, Diamonds)
let test_card15 = (Number 3, Diamonds)
let test_card16 = (Number 4, Diamonds)
let test_card17 = (Number 6, Diamonds)
let test_card18 = (Number 7, Diamonds)
let test_card19 = (Number 8, Diamonds)
let test_card20 = (Number 9, Diamonds)
let test_card21 = (Jack, Diamonds)
let test_card22 = (Queen, Diamonds)
let test_card23 = (Ace, Hearts)
let test_card24 = (Number 3, Hearts)
let test_card25 = (Number 4, Hearts)
let test_card26 = (Number 5, Hearts)
let test_card27 = (Number 7, Hearts)
let test_card28 = (Number 8, Hearts)
let test_card29 = (Number 9, Hearts)
let test_card30 = (Number 10, Hearts)
let test_card31 = (Jack, Hearts)
let test_card32 = (King, Hearts)
let test_card33 = (Ace, Clubs)
let test_card34 = (Number 2, Clubs)
let test_card35 = (Number 4, Clubs)
let test_card36 = (Number 5, Clubs)
let test_card37 = (Number 6, Clubs)
let test_card38 = (Number 9, Clubs)
let test_card39 = (Number 10, Clubs)
let test_card40 = (Jack, Clubs)
let test_card41 = (Queen, Clubs)
let test_card42 = (King, Clubs)
let test_card43 = (Ace, Spades)
let test_card44 = (Number 2, Spades)
let test_card45 = (Number 3, Spades)
let test_card46 = (Number 5, Spades)
let test_card47 = (Number 6, Spades)
let test_card48 = (Number 7, Spades)
let test_card49 = (Number 8, Spades)
let test_card50 = (Number 10, Spades)
let test_card51 = (Queen, Spades)
let test_card52 = (King, Spades)
let emptydeck = empty

let sampleDeck0 =
  [
    (Number 4, Hearts);
    (Number 8, Diamonds);
    (Number 10, Spades);
    (Number 2, Clubs);
  ]

let sampleDeck1 =
  [ (Ace, Clubs); (Number 3, Spades); (Number 4, Hearts); (Number 7, Diamonds) ]

let sampleDeck2 =
  [
    (Ace, Clubs);
    (Number 4, Clubs);
    (Ace, Spades);
    (Number 10, Hearts);
    (Number 6, Clubs);
  ]

let sampleDeck3 =
  [
    (Ace, Clubs);
    (Number 7, Hearts);
    (Ace, Spades);
    (Number 5, Diamonds);
    (Ace, Diamonds);
    (Number 2, Hearts);
  ]

let sampleDeck4 =
  [ (Ace, Diamonds); (Ace, Hearts); (Ace, Clubs); (Ace, Spades) ]

let fullDeck =
  [
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
  ]

let int_of_val_test (name : string) (v : value) (i : int) : test =
  name >:: fun _ -> assert_equal (int_of_val v) i

let suit_to_string_test (name : string) (s : suit) (suitstring : string) : test
    =
  name >:: fun _ -> assert_equal (suit_to_string s) suitstring

let value_to_string_test (name : string) (v : value) (valuestring : string) :
    test =
  name >:: fun _ -> assert_equal (value_to_string v) valuestring

let card_to_string_test (name : string) (c : card) (cardstring : string) : test
    =
  name >:: fun _ -> assert_equal (card_to_string c) cardstring

let num_aces_test (name : string) (d : deck) (aces : int) : test =
  name >:: fun _ -> assert_equal (num_aces d) aces

let deduct_test (name : string) (score : int) (numace : int) (post_deduct : int)
    : test =
  name >:: fun _ -> assert_equal (deduct score numace) post_deduct

let calculate_score_of_card_test (name : string) (c : card) (score : int) : test
    =
  name >:: fun _ -> assert_equal (calculate_score_of_card c) score

let calculate_score_test (name : string) (flop : deck) (acc : int)
    (score_calc : int) : test =
  name >:: fun _ -> assert_equal (calculate_score flop acc) score_calc

let check_score_test (name : string) (flop : deck) (score_adj : int) : test =
  name >:: fun _ -> assert_equal (check_score flop) score_adj

let card_to_pic_test (name : string) (c : card) (card_picked : string) : test =
  name >:: fun _ -> assert_equal (card_to_pic c) card_picked

let deck_to_string_test (name : string) (d : deck) (deck_string : string) : test
    =
  name >:: fun _ -> assert_equal (deck_to_string d) deck_string

let int_of_val_tests =
  [
    int_of_val_test "The value of an Ace is 11" Ace 11;
    int_of_val_test "The value of a face card is 10" King 10;
    int_of_val_test "The value of a face card is 10" Queen 10;
    int_of_val_test "The value of a face card is 10" Jack 10;
    int_of_val_test "The value of a number card is the number on the card"
      (Number 2) 2;
    int_of_val_test "The value of a number card is the number on the card"
      (Number 3) 3;
    int_of_val_test "The value of a number card is the number on the card"
      (Number 4) 4;
    int_of_val_test "The value of a number card is the number on the card"
      (Number 5) 5;
    int_of_val_test "The value of a number card is the number on the card"
      (Number 6) 6;
    int_of_val_test "The value of a number card is the number on the card"
      (Number 7) 7;
    int_of_val_test "The value of a number card is the number on the card"
      (Number 8) 8;
    int_of_val_test "The value of a number card is the number on the card"
      (Number 9) 9;
    int_of_val_test "The value of a number card is the number on the card"
      (Number 10) 10;
  ]

let suit_to_string_tests =
  [
    suit_to_string_test "The Spades suit should return the string 'Spades'"
      Spades "Spades";
    suit_to_string_test "The Diamonds suit should return the string 'Diamonds'"
      Diamonds "Diamonds";
    suit_to_string_test "The Clubs suit should return the string 'Clubs' " Clubs
      "Clubs";
    suit_to_string_test "The Hearts suit should return the string 'Hearts' "
      Hearts "Hearts";
  ]

let value_to_string_tests =
  [
    value_to_string_test "The Ace value should return the string 'Ace'" Ace
      "Ace";
    value_to_string_test "The Queen value should return the string 'Queen'"
      Queen "Queen";
    value_to_string_test "The Jack value should return the string 'Jack'" Jack
      "Jack";
    value_to_string_test "The King value should return the string 'King'" King
      "King";
    value_to_string_test
      "The value of any non-face card should return the string of its number"
      (Number 2) "2";
    value_to_string_test
      "The value of any non-face card should return the string of its number"
      (Number 3) "3";
    value_to_string_test
      "The value of any non-face card should return the string of its number"
      (Number 4) "4";
    value_to_string_test
      "The value of any non-face card should return the string of its number"
      (Number 5) "5";
    value_to_string_test
      "The value of any non-face card should return the string of its number"
      (Number 6) "6";
    value_to_string_test
      "The value of any non-face card should return the string of its number"
      (Number 7) "7";
    value_to_string_test
      "The value of any non-face card should return the string of its number"
      (Number 8) "8";
    value_to_string_test
      "The value of any non-face card should return the string of its number"
      (Number 9) "9";
    value_to_string_test
      "The value of any non-face card should return the string of its number"
      (Number 10) "10";
  ]

let card_to_string_tests =
  [
    card_to_string_test
      "The card_to_string of test_card1 should return 'Ace of Diamonds' "
      test_card1 "Ace of Diamonds";
    card_to_string_test
      "The card_to_string of test_card2 should return 'Queen of Hearts' "
      test_card2 "Queen of Hearts";
    card_to_string_test
      "The card_to_string of test_card3 should return '8 of Clubs'" test_card3
      "8 of Clubs";
    card_to_string_test
      "The card_to_string of test_card4 should return 'Jack of Spades' "
      test_card4 "Jack of Spades";
    card_to_string_test
      "The card_to_string of test_card5 should return '10 of Diamonds' "
      test_card5 "10 of Diamonds";
    card_to_string_test
      "The card_to_string of test_card6 should return '2 of Hearts' " test_card6
      "2 of Hearts";
    card_to_string_test
      "The card_to_string of test_card7 should be '3 of Clubs' " test_card7
      "3 of Clubs";
    card_to_string_test
      "The card_to_string of test_card8 should be '4 of Spades' " test_card8
      "4 of Spades";
    card_to_string_test
      "The card_to_string of test_card9 should be '5 of Diamonds'" test_card9
      "5 of Diamonds";
    card_to_string_test
      "The card_to_string of test_card10 should be '6 of Hearts'" test_card10
      "6 of Hearts";
    card_to_string_test
      "The card_to_string of test_card11 should be '7 of Clubs' " test_card11
      "7 of Clubs";
    card_to_string_test
      "The card_to_string of test_card12 should be '9 of Spades' " test_card12
      "9 of Spades";
    card_to_string_test
      "The card_to_string of test_card13 should be 'King of Diamonds' "
      test_card13 "King of Diamonds";
    card_to_string_test
      "The card_to_string of test_card14 should be '2 of Diamonds' " test_card14
      "2 of Diamonds";
    card_to_string_test
      "The card_to_string of test_card15 should be '3 of Diamonds' " test_card15
      "3 of Diamonds";
    card_to_string_test
      "The card_to_string of test_card16 should be '4 of Diamonds' " test_card16
      "4 of Diamonds";
    card_to_string_test
      "The card_to_string of test_card17 should be '6 of Diamonds' " test_card17
      "6 of Diamonds";
    card_to_string_test
      "The card_to_string of test_card18 should be '7 of Diamonds' " test_card18
      "7 of Diamonds";
    card_to_string_test
      "The card_to_string of test_card19 should be '8 of Diamonds' " test_card19
      "8 of Diamonds";
    card_to_string_test
      "The card_to_string of test_card20 should be '9 of Diamonds' " test_card20
      "9 of Diamonds";
    card_to_string_test
      "The card_to_string of test_card21 should be 'Jack of Diamonds' "
      test_card21 "Jack of Diamonds";
    card_to_string_test
      "The card_to_string of test_card22 should be 'Queen of Diamonds' "
      test_card22 "Queen of Diamonds";
    card_to_string_test
      "The card_to_string of test_card23 should be 'Ace of Hearts' " test_card23
      "Ace of Hearts";
    card_to_string_test
      "The card_to_string of test_card24 should be '3 of Hearts' " test_card24
      "3 of Hearts";
    card_to_string_test
      "The card_to_string of test_card25 should be '4 of Hearts' " test_card25
      "4 of Hearts";
    card_to_string_test
      "The card_to_string of test_card26 should be '5 of Hearts' " test_card26
      "5 of Hearts";
    card_to_string_test
      "The card_to_string of test_card27 should be '7 of Hearts' " test_card27
      "7 of Hearts";
    card_to_string_test
      "The card_to_string of test_card28 should be '8 of Hearts' " test_card28
      "8 of Hearts";
    card_to_string_test
      "The card_to_string of test_card29 should be '9 of Hearts' " test_card29
      "9 of Hearts";
    card_to_string_test
      "The card_to_string of test_card30 should be '10 of Hearts' " test_card30
      "10 of Hearts";
    card_to_string_test
      "The card_to_string of test_card31 should be 'Jack of Hearts' "
      test_card31 "Jack of Hearts";
    card_to_string_test
      "The card_to_string of test_card32 should be 'King of Hearts' "
      test_card32 "King of Hearts";
    card_to_string_test
      "The card_to_string of test_card33 should be 'Ace of Clubs' " test_card33
      "Ace of Clubs";
    card_to_string_test
      "The card_to_string of test_card34 should be '2 of Clubs' " test_card34
      "2 of Clubs";
    card_to_string_test
      "The card_to_string of test_card35 should be '4 of Clubs' " test_card35
      "4 of Clubs";
    card_to_string_test
      "The card_to_string of test_card36 should be '5 of Clubs' " test_card36
      "5 of Clubs";
    card_to_string_test
      "The card_to_string of test_card37 should be '6 of Clubs' " test_card37
      "6 of Clubs";
    card_to_string_test
      "The card_to_string of test_card38 should be '9 of Clubs' " test_card38
      "9 of Clubs";
    card_to_string_test
      "The card_to_string of test_card39 should be '10 of Clubs' " test_card39
      "10 of Clubs";
    card_to_string_test
      "The card_to_string of test_card40 should be 'Jack of Clubs' " test_card40
      "Jack of Clubs";
    card_to_string_test
      "The card_to_string of test_card41 should be 'Queen of Clubs' "
      test_card41 "Queen of Clubs";
    card_to_string_test
      "The card_to_string of test_card42 should be 'King of Clubs' " test_card42
      "King of Clubs";
    card_to_string_test
      "The card_to_string of test_card43 should be 'Ace of Spades' " test_card43
      "Ace of Spades";
    card_to_string_test
      "The card_to_string of test_card44 should be '2 of Spades' " test_card44
      "2 of Spades";
    card_to_string_test
      "The card_to_string of test_card45 should be '3 of Spades' " test_card45
      "3 of Spades";
    card_to_string_test
      "The card_to_string of test_card46 should be '5 of Spades' " test_card46
      "5 of Spades";
    card_to_string_test
      "The card_to_string of test_card47 should be '6 of Spades' " test_card47
      "6 of Spades";
    card_to_string_test
      "The card_to_string of test_card48 should be '7 of Spades' " test_card48
      "7 of Spades";
    card_to_string_test
      "The card_to_string of test_card49 should be '8 of Spades' " test_card49
      "8 of Spades";
    card_to_string_test
      "The card_to_string of test_card50 should be '10 of Spades' " test_card50
      "10 of Spades";
    card_to_string_test
      "The card_to_string of test_card51 should be 'Queen of Spades' "
      test_card51 "Queen of Spades";
    card_to_string_test
      "The card_to_string of test_card52 should be 'King of Spades' "
      test_card52 "King of Spades";
  ]

let num_aces_tests =
  [
    num_aces_test "The number of aces in the emptydeck is 0" emptydeck 0;
    num_aces_test "The number of aces in sampleDeck0 is 0" sampleDeck0 0;
    num_aces_test "The number of aces in sampleDeck1 is 1" sampleDeck1 1;
    num_aces_test "The number of aces in sampleDeck2 is 2" sampleDeck2 2;
    num_aces_test "The number of aces in sampleDeck3 is 3" sampleDeck3 3;
    num_aces_test "The number of aces in sampleDeck4 is 4" sampleDeck4 4;
    num_aces_test "The number of aces in fullDeck is 4" fullDeck 4;
  ]

let deduct_tests =
  [
    deduct_test "The score should be the same if it is under 21" 20 4 20;
    deduct_test
      "The score should be 12 less if the score is 22 and there is 1 ace" 22 1
      12;
    deduct_test
      "The score should remain the same if the score is 22 and ther are no aces"
      22 0 22;
    deduct_test
      "The score should be 30 less if the score is 42 and there are 4 aces" 42 4
      12;
  ]

let calculate_score_of_card_tests =
  [
    calculate_score_of_card_test "The score of a test_card1 should be 11"
      test_card1 11;
    calculate_score_of_card_test "The score of test_card2 should be 10"
      test_card2 10;
    calculate_score_of_card_test "The score of test_card3 should be 8"
      test_card3 8;
    calculate_score_of_card_test "The score of test_card4 should be 10"
      test_card4 10;
    calculate_score_of_card_test "The score of test_card5 should be 10"
      test_card5 10;
    calculate_score_of_card_test "The score of test_card6 should be 2"
      test_card6 2;
    calculate_score_of_card_test "The score of test_card7 should be 3"
      test_card7 3;
    calculate_score_of_card_test "The score of test_card8 should be 4"
      test_card8 4;
    calculate_score_of_card_test "The score of test_card9 should be 5"
      test_card9 5;
    calculate_score_of_card_test "The score of test_card10 should be 6"
      test_card10 6;
    calculate_score_of_card_test "The score of test_card11 should be 7"
      test_card11 7;
    calculate_score_of_card_test "The score of test_card12 should be 9"
      test_card12 9;
    calculate_score_of_card_test "The score of test_card13 should be 10"
      test_card13 10;
  ]

let calculate_score_tests =
  [
    calculate_score_test "The score of the emptydeck should be 0" emptydeck 0 0;
    calculate_score_test "The score of sampleDeck0 should be 24" sampleDeck0 0
      24;
    calculate_score_test "The score of sampleDeck1 should be 25" sampleDeck1 0
      25;
    calculate_score_test "The score of sampleDeck2 should be 42" sampleDeck2 0
      42;
    calculate_score_test "The score of sampleDeck3 should be 47" sampleDeck3 0
      47;
    calculate_score_test "The score of sampleDeck4 should be 44" sampleDeck4 0
      44;
    calculate_score_test "The score of fullDeck should be 380" fullDeck 0 380;
  ]

let check_score_tests =
  [
    check_score_test "The score of the emptydeck should be 0" emptydeck 0;
    check_score_test "The score of sampleDeck0 should be 24" sampleDeck0 24;
    check_score_test "The score of sampleDeck1 should be 15" sampleDeck1 15;
    check_score_test "The score of sampleDeck2 should be 22" sampleDeck2 22;
    check_score_test "The score of sampleDeck3 should be 17" sampleDeck3 17;
    check_score_test "The score of sampleDeck4 should be 14" sampleDeck4 14;
    check_score_test "The score of fullDeck should be 340" fullDeck 340;
  ]

let card_to_pic_tests =
  [
    card_to_pic_test
      "The card picked for test_card1 should return the string representation \
       of the Ace of Diamonds"
      test_card1 "   ___\n  |A  |\n  | <>|\n  |__A|";
    card_to_pic_test
      "The card picked for test_card2 should return the string representation \
       of the Queen of Hearts"
      test_card2 "   ___\n  |Q  |\n  | <3|\n  |__Q|";
    card_to_pic_test
      "The card picked for test_card3 should return the string representaiton \
       of the Eight of Clubs"
      test_card3 "   ___\n  |8  |\n  | -@|\n  |__8|";
    card_to_pic_test
      "The card picked for test_card4 should return the string representation \
       of the Jack of Spades"
      test_card4 "   ___\n  |J  |\n  | -D|\n  |__J|";
    card_to_pic_test
      "The card picked for test_card5 should return the string representation \
       of the Ten of Diamonds"
      test_card5 "   ___\n  |10 |\n  | <>|\n  |_10|";
    card_to_pic_test
      "The card picked for test_card6 should return the string representation \
       of the Two of Hearts"
      test_card6 "   ___\n  |2  |\n  | <3|\n  |__2|";
    card_to_pic_test
      "The card picked for test_card7 should return the string representation \
       of the Three of Clubs"
      test_card7 "   ___\n  |3  |\n  | -@|\n  |__3|";
    card_to_pic_test
      "The card picked for test_card8 should return the string representation \
       of the Four of Spades"
      test_card8 "   ___\n  |4  |\n  | -D|\n  |__4|";
    card_to_pic_test
      "The card picked for test_card9 should return the string representation \
       of the Five of Diamonds"
      test_card9 "   ___\n  |5  |\n  | <>|\n  |__5|";
    card_to_pic_test
      "The card picked for test_card10 should return the string representation \
       of the Six of Hearts"
      test_card10 "   ___\n  |6  |\n  | <3|\n  |__6|";
    card_to_pic_test
      "The card picked for test_card11 should return the string representation \
       of the Seven of Clubs"
      test_card11 "   ___\n  |7  |\n  | -@|\n  |__7|";
    card_to_pic_test
      "The card picked for test_card12 should return the string representation \
       of the Nine of Spades"
      test_card12 "   ___\n  |9  |\n  | -D|\n  |__9|";
    card_to_pic_test
      "The card picked for test_card13 should return the string representation \
       of the King of Diamonds"
      test_card13 "   ___\n  |K  |\n  | <>|\n  |__K|";
  ]

let deck_to_string_tests =
  [
    deck_to_string_test "The emptydeck should return an empty string" emptydeck
      "";
    deck_to_string_test
      "The sampleDeck0 should return the string\n\
      \       representation of the deck" sampleDeck0
      ("4 of Hearts" ^ "\n " ^ "   ___\n  |4  |\n  | <3|\n  |__4|" ^ "\n"
     ^ "8 of Diamonds" ^ "\n " ^ "   ___\n  |8  |\n  | <>|\n  |__8|" ^ "\n"
     ^ "10 of Spades" ^ "\n " ^ "   ___\n  |10 |\n  | -D|\n  |_10|" ^ "\n"
     ^ "2 of Clubs" ^ "\n " ^ "   ___\n  |2  |\n  | -@|\n  |__2|" ^ "\n");
    deck_to_string_test
      "The sampleDeck1 should return the string representation of the deck"
      sampleDeck1
      ("Ace of Clubs" ^ "\n " ^ "   ___\n  |A  |\n  | -@|\n  |__A|" ^ "\n"
     ^ "3 of Spades" ^ "\n " ^ "   ___\n  |3  |\n  | -D|\n  |__3|" ^ "\n"
     ^ "4 of Hearts" ^ "\n " ^ "   ___\n  |4  |\n  | <3|\n  |__4|" ^ "\n"
     ^ "7 of Diamonds" ^ "\n " ^ "   ___\n  |7  |\n  | <>|\n  |__7|" ^ "\n");
    deck_to_string_test
      "The samplDeck2 should return the string representation of the deck"
      sampleDeck2
      ("Ace of Clubs" ^ "\n " ^ "   ___\n  |A  |\n  | -@|\n  |__A|" ^ "\n"
     ^ "4 of Clubs" ^ "\n " ^ "   ___\n  |4  |\n  | -@|\n  |__4|" ^ "\n"
     ^ "Ace of Spades" ^ "\n " ^ "   ___\n  |A  |\n  | -D|\n  |__A|" ^ "\n"
     ^ "10 of Hearts" ^ "\n " ^ "   ___\n  |10 |\n  | <3|\n  |_10|" ^ "\n"
     ^ "6 of Clubs" ^ "\n " ^ "   ___\n  |6  |\n  | -@|\n  |__6|" ^ "\n");
    deck_to_string_test
      "The sampleDeck3 should return the string representation of the deck"
      sampleDeck3
      ("Ace of Clubs" ^ "\n " ^ "   ___\n  |A  |\n  | -@|\n  |__A|" ^ "\n"
     ^ "7 of Hearts" ^ "\n " ^ "   ___\n  |7  |\n  | <3|\n  |__7|" ^ "\n"
     ^ "Ace of Spades" ^ "\n " ^ "   ___\n  |A  |\n  | -D|\n  |__A|" ^ "\n"
     ^ "5 of Diamonds" ^ "\n " ^ "   ___\n  |5  |\n  | <>|\n  |__5|" ^ "\n"
     ^ "Ace of Diamonds" ^ "\n " ^ "   ___\n  |A  |\n  | <>|\n  |__A|" ^ "\n"
     ^ "2 of Hearts" ^ "\n " ^ "   ___\n  |2  |\n  | <3|\n  |__2|" ^ "\n");
    deck_to_string_test
      "The sampleDeck4 should return the string representation of the deck"
      sampleDeck4
      ("Ace of Diamonds" ^ "\n " ^ "   ___\n  |A  |\n  | <>|\n  |__A|" ^ "\n"
     ^ "Ace of Hearts" ^ "\n " ^ "   ___\n  |A  |\n  | <3|\n  |__A|" ^ "\n"
     ^ "Ace of Clubs" ^ "\n " ^ "   ___\n  |A  |\n  | -@|\n  |__A|" ^ "\n"
     ^ "Ace of Spades" ^ "\n " ^ "   ___\n  |A  |\n  | -D|\n  |__A|" ^ "\n");
    deck_to_string_test
      "The fullDeck should return the string representation of the entire dek"
      fullDeck
      ("10 of Spades" ^ "\n " ^ "   ___\n  |10 |\n  | -D|\n  |_10|" ^ "\n"
     ^ "10 of Diamonds" ^ "\n " ^ "   ___\n  |10 |\n  | <>|\n  |_10|" ^ "\n"
     ^ "10 of Clubs" ^ "\n " ^ "   ___\n  |10 |\n  | -@|\n  |_10|" ^ "\n"
     ^ "10 of Hearts" ^ "\n " ^ "   ___\n  |10 |\n  | <3|\n  |_10|" ^ "\n"
     ^ "9 of Spades" ^ "\n " ^ "   ___\n  |9  |\n  | -D|\n  |__9|" ^ "\n"
     ^ "9 of Diamonds" ^ "\n " ^ "   ___\n  |9  |\n  | <>|\n  |__9|" ^ "\n"
     ^ "9 of Clubs" ^ "\n " ^ "   ___\n  |9  |\n  | -@|\n  |__9|" ^ "\n"
     ^ "9 of Hearts" ^ "\n " ^ "   ___\n  |9  |\n  | <3|\n  |__9|" ^ "\n"
     ^ "8 of Spades" ^ "\n " ^ "   ___\n  |8  |\n  | -D|\n  |__8|" ^ "\n"
     ^ "8 of Diamonds" ^ "\n " ^ "   ___\n  |8  |\n  | <>|\n  |__8|" ^ "\n"
     ^ "8 of Clubs" ^ "\n " ^ "   ___\n  |8  |\n  | -@|\n  |__8|" ^ "\n"
     ^ "8 of Hearts" ^ "\n " ^ "   ___\n  |8  |\n  | <3|\n  |__8|" ^ "\n"
     ^ "7 of Spades" ^ "\n " ^ "   ___\n  |7  |\n  | -D|\n  |__7|" ^ "\n"
     ^ "7 of Diamonds" ^ "\n " ^ "   ___\n  |7  |\n  | <>|\n  |__7|" ^ "\n"
     ^ "7 of Clubs" ^ "\n " ^ "   ___\n  |7  |\n  | -@|\n  |__7|" ^ "\n"
     ^ "7 of Hearts" ^ "\n " ^ "   ___\n  |7  |\n  | <3|\n  |__7|" ^ "\n"
     ^ "6 of Spades" ^ "\n " ^ "   ___\n  |6  |\n  | -D|\n  |__6|" ^ "\n"
     ^ "6 of Diamonds" ^ "\n " ^ "   ___\n  |6  |\n  | <>|\n  |__6|" ^ "\n"
     ^ "6 of Clubs" ^ "\n " ^ "   ___\n  |6  |\n  | -@|\n  |__6|" ^ "\n"
     ^ "6 of Hearts" ^ "\n " ^ "   ___\n  |6  |\n  | <3|\n  |__6|" ^ "\n"
     ^ "5 of Spades" ^ "\n " ^ "   ___\n  |5  |\n  | -D|\n  |__5|" ^ "\n"
     ^ "5 of Diamonds" ^ "\n " ^ "   ___\n  |5  |\n  | <>|\n  |__5|" ^ "\n"
     ^ "5 of Clubs" ^ "\n " ^ "   ___\n  |5  |\n  | -@|\n  |__5|" ^ "\n"
     ^ "5 of Hearts" ^ "\n " ^ "   ___\n  |5  |\n  | <3|\n  |__5|" ^ "\n"
     ^ "4 of Spades" ^ "\n " ^ "   ___\n  |4  |\n  | -D|\n  |__4|" ^ "\n"
     ^ "4 of Diamonds" ^ "\n " ^ "   ___\n  |4  |\n  | <>|\n  |__4|" ^ "\n"
     ^ "4 of Clubs" ^ "\n " ^ "   ___\n  |4  |\n  | -@|\n  |__4|" ^ "\n"
     ^ "4 of Hearts" ^ "\n " ^ "   ___\n  |4  |\n  | <3|\n  |__4|" ^ "\n"
     ^ "3 of Spades" ^ "\n " ^ "   ___\n  |3  |\n  | -D|\n  |__3|" ^ "\n"
     ^ "3 of Diamonds" ^ "\n " ^ "   ___\n  |3  |\n  | <>|\n  |__3|" ^ "\n"
     ^ "3 of Clubs" ^ "\n " ^ "   ___\n  |3  |\n  | -@|\n  |__3|" ^ "\n"
     ^ "3 of Hearts" ^ "\n " ^ "   ___\n  |3  |\n  | <3|\n  |__3|" ^ "\n"
     ^ "2 of Spades" ^ "\n " ^ "   ___\n  |2  |\n  | -D|\n  |__2|" ^ "\n"
     ^ "2 of Diamonds" ^ "\n " ^ "   ___\n  |2  |\n  | <>|\n  |__2|" ^ "\n"
     ^ "2 of Clubs" ^ "\n " ^ "   ___\n  |2  |\n  | -@|\n  |__2|" ^ "\n"
     ^ "2 of Hearts" ^ "\n " ^ "   ___\n  |2  |\n  | <3|\n  |__2|" ^ "\n"
     ^ "Jack of Spades" ^ "\n " ^ "   ___\n  |J  |\n  | -D|\n  |__J|" ^ "\n"
     ^ "Jack of Diamonds" ^ "\n " ^ "   ___\n  |J  |\n  | <>|\n  |__J|" ^ "\n"
     ^ "Jack of Clubs" ^ "\n " ^ "   ___\n  |J  |\n  | -@|\n  |__J|" ^ "\n"
     ^ "Jack of Hearts" ^ "\n " ^ "   ___\n  |J  |\n  | <3|\n  |__J|" ^ "\n"
     ^ "Queen of Spades" ^ "\n " ^ "   ___\n  |Q  |\n  | -D|\n  |__Q|" ^ "\n"
     ^ "Queen of Diamonds" ^ "\n " ^ "   ___\n  |Q  |\n  | <>|\n  |__Q|" ^ "\n"
     ^ "Queen of Clubs" ^ "\n " ^ "   ___\n  |Q  |\n  | -@|\n  |__Q|" ^ "\n"
     ^ "Queen of Hearts" ^ "\n " ^ "   ___\n  |Q  |\n  | <3|\n  |__Q|" ^ "\n"
     ^ "King of Spades" ^ "\n " ^ "   ___\n  |K  |\n  | -D|\n  |__K|" ^ "\n"
     ^ "King of Diamonds" ^ "\n " ^ "   ___\n  |K  |\n  | <>|\n  |__K|" ^ "\n"
     ^ "King of Clubs" ^ "\n " ^ "   ___\n  |K  |\n  | -@|\n  |__K|" ^ "\n"
     ^ "King of Hearts" ^ "\n " ^ "   ___\n  |K  |\n  | <3|\n  |__K|" ^ "\n"
     ^ "Ace of Spades" ^ "\n " ^ "   ___\n  |A  |\n  | -D|\n  |__A|" ^ "\n"
     ^ "Ace of Diamonds" ^ "\n " ^ "   ___\n  |A  |\n  | <>|\n  |__A|" ^ "\n"
     ^ "Ace of Clubs" ^ "\n " ^ "   ___\n  |A  |\n  | -@|\n  |__A|" ^ "\n"
     ^ "Ace of Hearts" ^ "\n " ^ "   ___\n  |A  |\n  | <3|\n  |__A|" ^ "\n");
  ]

let suite =
  "Test suite for final project"
  >::: List.flatten
         [
           int_of_val_tests;
           suit_to_string_tests;
           value_to_string_tests;
           card_to_string_tests;
           num_aces_tests;
           deduct_tests;
           calculate_score_of_card_tests;
           calculate_score_tests;
           check_score_tests;
           card_to_pic_tests;
           deck_to_string_tests;
         ]

let _ = run_test_tt_main suite
