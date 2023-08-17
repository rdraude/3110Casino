open OUnit2
(**Teset Plan:

   The functions ran the key logic behind the game were tested by OUnit2 in this
   main file. The other fnctions that provided the interactive interface with
   the player (and had output unit) were teseted manually by manually playing
   the game.

   For this testing file, the Roulette module was tested by OUni2. Aditionally,
   these test cases were developed by blackbox and glassbox testing. The
   implementor loooked at the functions first to understand how it worked, and
   then developed test cases around that.

   This testing approach demonstates the correctness of the system due to the
   extensiveness of the tests. The majority of the functions were run on the
   differnt types of bets and wheels to eliminate any inconsistencies and to
   ensure that the logic behind calculating score was done correctly. Debugging
   the code this way was helpful, as numerous small errors were identified and
   subsequently resolved.*)

open Roul
open Roulette
(* open Parse *)

(** Testing file*)

(**let new_game = Roulette.start_game*)
let bet1 = Square (Black 2)

let gm1 = { score = 50 }
let sqgs = Square (Green Single)
let sr1 = Square (Red 1)
let sb2 = Square (Black 2)
let sr3 = Square (Red 3)
let sb4 = Square (Black 4)
let sr5 = Square (Red 5)
let sb6 = Square (Black 6)
let sr7 = Square (Red 7)
let sb8 = Square (Black 8)
let sr9 = Square (Red 9)
let sb10 = Square (Black 10)
let sb11 = Square (Black 11)
let sr12 = Square (Red 12)
let sb13 = Square (Black 13)
let sr14 = Square (Red 14)
let sb15 = Square (Black 15)
let sr16 = Square (Red 16)
let sb17 = Square (Black 17)
let sr18 = Square (Red 18)
let sr19 = Square (Red 19)
let sb20 = Square (Black 20)
let sr21 = Square (Red 21)
let sb22 = Square (Black 22)
let sr23 = Square (Red 23)
let sb24 = Square (Black 24)
let sr25 = Square (Red 25)
let sb26 = Square (Black 26)
let sr27 = Square (Red 27)
let sb28 = Square (Black 28)
let sb29 = Square (Black 29)
let sr30 = Square (Red 30)
let sb31 = Square (Black 31)
let sr32 = Square (Red 32)
let sb33 = Square (Black 33)
let sr34 = Square (Red 34)
let sb35 = Square (Black 35)
let sr36 = Square (Red 36)
let sgdbl = Square (Green Double)
let cr = Color Red
let cb = Color Black
let cg = Color Green
let d1_12 = Dozen One_to_Twelve
let d13_24 = Dozen Thirteen_to_Twentyfour
let d25_36 = Dozen Twentyfive_to_Thirtysix
let hf = Half First
let hs = Half Second
let w1 = 50
let w2 = 30
let w3 = 25
let (whe1 : wheel) = Red 16
let (wgs : wheel) = Green Single
let (wr1 : wheel) = Red 1
let (wb2 : wheel) = Black 2
let (wr3 : wheel) = Red 3
let (wb4 : wheel) = Black 4
let (wr5 : wheel) = Red 5
let (wb6 : wheel) = Black 6
let (wr7 : wheel) = Red 7
let (wb8 : wheel) = Black 8
let (wr9 : wheel) = Red 9
let (wb10 : wheel) = Black 10
let (wb11 : wheel) = Black 11
let (wr12 : wheel) = Red 12
let (wb13 : wheel) = Black 13
let (wr14 : wheel) = Red 14
let (wb15 : wheel) = Black 15
let (wr16 : wheel) = Red 16
let (wb17 : wheel) = Black 17
let (wr18 : wheel) = Red 18
let (wr19 : wheel) = Red 19
let (wb20 : wheel) = Black 20
let (wr21 : wheel) = Red 21
let (wb22 : wheel) = Black 22
let (wr23 : wheel) = Red 23
let (wb24 : wheel) = Black 24
let (wr25 : wheel) = Red 25
let (wb26 : wheel) = Black 26
let (wr27 : wheel) = Red 27
let (wb28 : wheel) = Black 28
let (wb29 : wheel) = Black 29
let (wr30 : wheel) = Red 30
let (wb31 : wheel) = Black 31
let (wr32 : wheel) = Red 32
let (wb33 : wheel) = Black 33
let (wr34 : wheel) = Red 34
let (wb35 : wheel) = Black 35
let (wr36 : wheel) = Red 36
let (wgdbl : wheel) = Green Double
let sos (str : string) = str

let nc_test (name : string) (b : bet) (wager : int) (result : wheel)
    (score : int) (exp : int) =
  name >:: fun _ ->
  assert_equal exp (number_checker b wager result score) ~printer:string_of_int

let cc_test (name : string) (b : bet) (wager : int) (result : wheel)
    (score : int) (exp : int) =
  name >:: fun _ ->
  assert_equal exp (color_checker b wager result score) ~printer:string_of_int

let dc_test (name : string) (b : bet) (wager : int) (result : wheel)
    (score : int) (exp : int) =
  name >:: fun _ ->
  assert_equal exp (dozen_checker b wager result score) ~printer:string_of_int

let calc_bet_test (name : string) (b : bet) (wager : int) (res : wheel)
    (score : int) (exp : int) =
  name >:: fun _ ->
  assert_equal exp (calculate_bet b wager res score) ~printer:string_of_int

let mstf_test (name : string) (str : string) (exp : bet) =
  name >:: fun _ -> assert_equal exp (match_string_to_fun str)

(**let match_string_to_error (name : string) (str : string) = name >:: fun _ ->
   assert_raises (match_string_to_fun str) (InvalidBet "Not a bet")*)

let res_to_string_test (name : string) (res : wheel) (exp : string) =
  name >:: fun _ -> assert_equal exp (res_to_string res) ~printer:sos

let roul_test (*: test list*) =
  [
    nc_test "number_checker sqgs " sqgs w1 whe1 100 50;
    nc_test "number_checker sr1 with changing bets" sr1 w2 whe1 100 70;
    nc_test "number_checker sb2 with changing bets" sb2 w3 whe1 100 75;
    nc_test "number_checker sr3 with changing bets" sr3 w1 whe1 100 50;
    nc_test "number_checker sb4 with changing bets" sb4 w2 whe1 100 70;
    nc_test "number_checker sr5 with changing bets" sr5 w3 whe1 100 75;
    nc_test "number_checker sb6 with changing bets" sb6 w1 whe1 100 50;
    nc_test "number_checker sr7 with changing bets" sr7 w2 whe1 100 70;
    nc_test "number_checker sb8 with changing bets" sb8 w3 whe1 100 75;
    nc_test "number_checker sr9 with changing bets" sr9 w1 whe1 100 50;
    nc_test "number_checker sb10 with changing bets" sb10 w2 whe1 100 70;
    nc_test "number_checker sb11 with changing bets" sb11 w3 whe1 100 75;
    nc_test "number_checker sr12 with changing bets" sr12 w1 whe1 100 50;
    nc_test "number_checker sb13 with changing bets" sb13 w2 whe1 100 70;
    nc_test "number_checker sr14 with changing bets" sr14 w3 whe1 100 75;
    nc_test "number_checker sb15 with changing bets" sb15 w1 whe1 100 50;
    nc_test "number_checker sr16 with changing bets" sr16 w2 whe1 100 1150;
    nc_test "number_checker sb17 with changing bets" sb17 w3 whe1 100 75;
    nc_test "number_checker sr18 with changing bets" sr18 w1 whe1 100 50;
    nc_test "number_checker sr19 with changing bets" sr19 w2 whe1 100 70;
    nc_test "number_checker sb20 with changing bets" sb20 w3 whe1 100 75;
    nc_test "number_checker sr21 with changing bets" sr21 w1 whe1 100 50;
    nc_test "number_checker sb22 with changing bets" sb22 w2 whe1 100 70;
    nc_test "number_checker sr23 with changing bets" sr23 w3 whe1 100 75;
    nc_test "number_checker sb24 with changing bets" sb24 w1 whe1 100 50;
    nc_test "number_checker sr25 with changing bets" sr25 w2 whe1 100 70;
    nc_test "number_checker sb26 with changing bets" sb26 w3 whe1 100 75;
    nc_test "number_checker sr27 with changing bets" sr27 w1 whe1 100 50;
    nc_test "number_checker sb28 with changing bets" sb28 w2 whe1 100 70;
    nc_test "number_checker sb29 with changing bets" sb29 w3 whe1 100 75;
    nc_test "number_checker sr30 with changing bets" sr30 w1 whe1 100 50;
    nc_test "number_checker sb31 with changing bets" sb31 w2 whe1 100 70;
    nc_test "number_checker sr32 with changing bets" sr32 w3 whe1 100 75;
    nc_test "number_checker sb33 with changing bets" sb33 w1 whe1 100 50;
    nc_test "number_checker sr34 with changing bets" sr34 w2 whe1 100 70;
    nc_test "number_checker sb35 with changing bets" sb35 w3 whe1 100 75;
    nc_test "number_checker sr36 with changing bets" sr36 w1 whe1 100 50;
    nc_test "number_checker sgdbl with changing bets" sgdbl w2 whe1 100 70;
    nc_test "number_checker cr with changing bets" cr w3 whe1 100 100;
    nc_test "number_checker cb with changing bets" cb w1 whe1 100 100;
    nc_test "number_checker cg with changing bets" cg w2 whe1 100 100;
    nc_test "number_checker d1_12 with changing bets" d1_12 w3 whe1 100 100;
    nc_test "number_checker d13_24 with changing bets" d13_24 w1 whe1 100 100;
    nc_test "number_checker d25_36 with changing bets" d25_36 w2 whe1 100 100;
    nc_test "number_checker hf w3 with changing bets" hf w3 whe1 100 100;
    nc_test "number_checker hs w1 with changing bets" hs w1 whe1 100 100;
    (*numbers with changing wheels*)
    nc_test "number_checker wgs with chanaging wheels" sqgs w1 wgs 100 1850;
    nc_test "number_checker wgdbl with changing wheels" sqgs w1 wgdbl 100 50;
    nc_test "number_checker wr1 " sqgs w1 wr1 100 50;
    nc_test "number_checker wb2 " sqgs w1 wb2 100 50;
    nc_test "number_checker wr3 " sqgs w1 wr3 100 50;
    nc_test "number_checker wb4 " sqgs w1 wb4 100 50;
    nc_test "number_checker wr5 " sqgs w1 wr5 100 50;
    nc_test "number_checker wb6 " sqgs w1 wb6 100 50;
    nc_test "number_checker wr7 " sqgs w1 wr7 100 50;
    nc_test "number_checker wb8 " sqgs w1 wb8 100 50;
    nc_test "number_checker wr9 " sqgs w1 wr9 100 50;
    nc_test "number_checker wb10 " sqgs w1 wb10 100 50;
    nc_test "number_checker wb11 " sqgs w1 wb11 100 50;
    nc_test "number_checker wr12 " sqgs w1 wr12 100 50;
    nc_test "number_checker wb13 " sqgs w1 wb13 100 50;
    nc_test "number_checker wr14 " sqgs w1 wr14 100 50;
    nc_test "number_checker wb15 " sqgs w1 wb15 100 50;
    nc_test "number_checker wr16 " sqgs w1 wr16 100 50;
    nc_test "number_checker wb17 " sqgs w1 wb17 100 50;
    nc_test "number_checker wr18 " sqgs w1 wr18 100 50;
    nc_test "number_checker wr19 " sqgs w1 wr19 100 50;
    nc_test "number_checker wb20 " sqgs w1 wb20 100 50;
    nc_test "number_checker wr21 " sqgs w1 wr21 100 50;
    nc_test "number_checker wb22 " sqgs w1 wb22 100 50;
    nc_test "number_checker wr23 " sqgs w1 wr23 100 50;
    nc_test "number_checker wb24 " sqgs w1 wb24 100 50;
    nc_test "number_checker wr25 " sqgs w1 wr25 100 50;
    nc_test "number_checker wb26 " sqgs w1 wb26 100 50;
    nc_test "number_checker wr27 " sqgs w1 wr27 100 50;
    nc_test "number_checker wb28 " sqgs w1 wb28 100 50;
    nc_test "number_checker wb29 " sqgs w1 wb29 100 50;
    nc_test "number_checker wr30 " sqgs w1 wr30 100 50;
    nc_test "number_checker wb31 " sqgs w1 wb31 100 50;
    nc_test "number_checker wr32 " sqgs w1 wr32 100 50;
    nc_test "number_checker wb33 " sqgs w1 wb33 100 50;
    nc_test "number_checker wr34 " sqgs w1 wr34 100 50;
    nc_test "number_checker wb35 " sqgs w1 wb35 100 50;
    nc_test "number_checker wr36 " sqgs w1 wr36 100 50;
    (*colors*)
    cc_test "color_checker sqgs " sqgs w1 whe1 100 100;
    cc_test "color_checker sr1 " sr1 w2 whe1 100 100;
    cc_test "color_checker sb2 " sb2 w3 whe1 100 100;
    cc_test "color_checker sr3 " sr3 w1 whe1 100 100;
    cc_test "color_checker sb4 " sb4 w2 whe1 100 100;
    cc_test "color_checker sr5 " sr5 w3 whe1 100 100;
    cc_test "color_checker sb6 " sb6 w1 whe1 100 100;
    cc_test "color_checker sr7 " sr7 w2 whe1 100 100;
    cc_test "color_checker sb8 " sb8 w3 whe1 100 100;
    cc_test "color_checker sr9 " sr9 w1 whe1 100 100;
    cc_test "color_checker sb10 " sb10 w2 whe1 100 100;
    cc_test "color_checker sb11 " sb11 w3 whe1 100 100;
    cc_test "color_checker sr12 " sr12 w1 whe1 100 100;
    cc_test "color_checker sb13 " sb13 w2 whe1 100 100;
    cc_test "color_checker sr14 " sr14 w3 whe1 100 100;
    cc_test "color_checker sb15 " sb15 w1 whe1 100 100;
    cc_test "color_checker sr16 " sr16 w2 whe1 100 100;
    cc_test "color_checker sb17 " sb17 w3 whe1 100 100;
    cc_test "color_checker sr18 " sr18 w1 whe1 100 100;
    cc_test "color_checker sr19 " sr19 w2 whe1 100 100;
    cc_test "color_checker sb20 " sb20 w3 whe1 100 100;
    cc_test "color_checker sr21 " sr21 w1 whe1 100 100;
    cc_test "color_checker sb22 " sb22 w2 whe1 100 100;
    cc_test "color_checker sr23 " sr23 w3 whe1 100 100;
    cc_test "color_checker sb24 " sb24 w1 whe1 100 100;
    cc_test "color_checker sr25 " sr25 w2 whe1 100 100;
    cc_test "color_checker sb26 " sb26 w3 whe1 100 100;
    cc_test "color_checker sr27 " sr27 w1 whe1 100 100;
    cc_test "color_checker sb28 " sb28 w2 whe1 100 100;
    cc_test "color_checker sb29 " sb29 w3 whe1 100 100;
    cc_test "color_checker sr30 " sr30 w1 whe1 100 100;
    cc_test "color_checker sb31 " sb31 w2 whe1 100 100;
    cc_test "color_checker sr32 " sr32 w3 whe1 100 100;
    cc_test "color_checker sb33 " sb33 w1 whe1 100 100;
    cc_test "color_checker sr34 " sr34 w2 whe1 100 100;
    cc_test "color_checker sb35 " sb35 w3 whe1 100 100;
    cc_test "color_checker sr36 " sr36 w1 whe1 100 100;
    cc_test "color_checker sgdbl" sgdbl w2 whe1 100 100;
    cc_test "color_checker cr" cr w3 whe1 100 125;
    cc_test "color_checker cb" cb w1 whe1 100 50;
    cc_test "color_checker cg" cg w2 whe1 100 70;
    cc_test "color_checker d1_12 " d1_12 w3 whe1 100 100;
    cc_test "color_checker d13_24" d13_24 w1 whe1 100 100;
    cc_test "color_checker d25_36" d25_36 w2 whe1 100 100;
    cc_test "color_checker hf w3 " hf w3 whe1 100 100;
    cc_test "color_checker hs w1 " hs w1 whe1 100 100;
    (*dozen checker*)
    dc_test "dozen_checker sqgs " sqgs w1 whe1 100 100;
    dc_test "dozen sr1 " sr1 w2 whe1 100 100;
    dc_test "dozen_checker sb2 " sb2 w3 whe1 100 100;
    dc_test "dozen_checker sr3 " sr3 w1 whe1 100 100;
    dc_test "dozen_checker sb4 " sb4 w2 whe1 100 100;
    dc_test "dozen_checker sr5 " sr5 w3 whe1 100 100;
    dc_test "dozen_checker sb6 " sb6 w1 whe1 100 100;
    dc_test "dozen_checker sr7 " sr7 w2 whe1 100 100;
    dc_test "dozen_checker sb8 " sb8 w3 whe1 100 100;
    dc_test "dozen_checker sr9 " sr9 w1 whe1 100 100;
    dc_test "dozen_checker sb10 " sb10 w2 whe1 100 100;
    dc_test "dozen_checker sb11 " sb11 w3 whe1 100 100;
    dc_test "dozen_checker sr12 " sr12 w1 whe1 100 100;
    dc_test "dozen_checker sb13 " sb13 w2 whe1 100 100;
    dc_test "dozen_checker sr14 " sr14 w3 whe1 100 100;
    dc_test "dozen_checker sb15 " sb15 w1 whe1 100 100;
    dc_test "dozen_checker sr16 " sr16 w2 whe1 100 100;
    dc_test "dozen_checker sb17 " sb17 w3 whe1 100 100;
    dc_test "dozen_checker sr18 " sr18 w1 whe1 100 100;
    dc_test "dozen_checker sr19 " sr19 w2 whe1 100 100;
    dc_test "dozen_checker sb20 " sb20 w3 whe1 100 100;
    dc_test "dozen_checker sr21 " sr21 w1 whe1 100 100;
    dc_test "dozen_checker sb22 " sb22 w2 whe1 100 100;
    dc_test "dozen_checker sr23 " sr23 w3 whe1 100 100;
    dc_test "dozen_checker sb24 " sb24 w1 whe1 100 100;
    dc_test "dozen_checker sr25 " sr25 w2 whe1 100 100;
    dc_test "dozen_checker sb26 " sb26 w3 whe1 100 100;
    dc_test "dozen_checker sr27 " sr27 w1 whe1 100 100;
    dc_test "dozen_checker sb28 " sb28 w2 whe1 100 100;
    dc_test "dozen_checker sb29 " sb29 w3 whe1 100 100;
    dc_test "dozen_checker sr30 " sr30 w1 whe1 100 100;
    dc_test "dozen_checker sb31 " sb31 w2 whe1 100 100;
    dc_test "dozen_checker sr32 " sr32 w3 whe1 100 100;
    dc_test "dozen_checker sb33 " sb33 w1 whe1 100 100;
    dc_test "dozen_checker sr34 " sr34 w2 whe1 100 100;
    dc_test "dozen_checker sb35 " sb35 w3 whe1 100 100;
    dc_test "dozen_checker sr36 " sr36 w1 whe1 100 100;
    dc_test "dozen_checker sgdbl" sgdbl w2 whe1 100 100;
    dc_test "dozen_checker cr" cr w3 whe1 100 100;
    dc_test "dozen_checker cb" cb w1 whe1 100 100;
    dc_test "dozen_checker cg" cg w2 whe1 100 100;
    dc_test "dozen_checker d1_12 " d1_12 w3 whe1 100 75;
    dc_test "dozen_checker d13_24" d13_24 w1 whe1 100 200;
    dc_test "dozen_checker d25_36" d25_36 w2 whe1 100 70;
    dc_test "dozen_checker hf w3 " hf w3 whe1 100 100;
    dc_test "dozen_checker hs w1 " hs w1 whe1 100 100;
    (*calc changing bets*)
    calc_bet_test "calc_bet test sqgs" sqgs w1 whe1 100 50;
    calc_bet_test "calc_bet test sr1 " sr1 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb2 " sb2 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr3 " sr3 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb4 " sb4 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr5 " sr5 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb6 " sb6 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr7 " sr7 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb8 " sb8 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr9 " sr9 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb10 " sb10 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb11 " sb11 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr12 " sr12 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb13 " sb13 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr14 " sr14 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb15 " sb15 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr16 " sr16 w1 whe1 100 1850;
    calc_bet_test "calc_bet test sb17 " sb17 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr18 " sr18 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr19 " sr19 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb20 " sb20 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr21 " sr21 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb22 " sb22 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr23 " sr23 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb24 " sb24 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr25 " sr25 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb26 " sb26 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr27 " sr27 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb28 " sb28 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb29 " sb29 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr30 " sr30 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb31 " sb31 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr32 " sr32 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb33 " sb33 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr34 " sr34 w1 whe1 100 50;
    calc_bet_test "calc_bet test sb35 " sb35 w1 whe1 100 50;
    calc_bet_test "calc_bet test sr36 " sr36 w1 whe1 100 50;
    (*calc changing wheels*)
    calc_bet_test "calc_bet test wr1 " sr1 w1 wr1 100 1850;
    calc_bet_test "calc_bet test wb2 " sr1 w2 wb2 100 70;
    calc_bet_test "calc_bet test wr3 " sr1 w2 wr3 100 70;
    calc_bet_test "calc_bet test wb4 " sr1 w2 wb4 100 70;
    calc_bet_test "calc_bet test wr5 " sr1 w2 wr5 100 70;
    calc_bet_test "calc_bet test wb6 " sr1 w2 wb6 100 70;
    calc_bet_test "calc_bet test wr7 " sr1 w2 wr7 100 70;
    calc_bet_test "calc_bet test wb8 " sr1 w2 wb8 100 70;
    calc_bet_test "calc_bet test wr9 " sr1 w2 wr9 100 70;
    calc_bet_test "calc_bet test wb10 " sr1 w2 wb10 100 70;
    calc_bet_test "calc_bet test wb11 " sr1 w2 wb11 100 70;
    calc_bet_test "calc_bet test wr12 " sr1 w2 wr12 100 70;
    calc_bet_test "calc_bet test wb13 " sr1 w2 wb13 100 70;
    calc_bet_test "calc_bet test wr14 " sr1 w2 wr14 100 70;
    calc_bet_test "calc_bet test wb15 " sr1 w2 wb15 100 70;
    calc_bet_test "calc_bet test wr16 " sr1 w2 wr16 100 70;
    calc_bet_test "calc_bet test wb17 " sr1 w2 wb17 100 70;
    calc_bet_test "calc_bet test wr18 " sr1 w2 wr18 100 70;
    calc_bet_test "calc_bet test wr19 " sr1 w2 wr19 100 70;
    calc_bet_test "calc_bet test wb20 " sr1 w2 wb20 100 70;
    calc_bet_test "calc_bet test wr21 " sr1 w2 wr21 100 70;
    calc_bet_test "calc_bet test wb22 " sr1 w2 wb22 100 70;
    calc_bet_test "calc_bet test wr23 " sr1 w2 wr23 100 70;
    calc_bet_test "calc_bet test wb24 " sr1 w2 wb24 100 70;
    calc_bet_test "calc_bet test wr25 " sr1 w2 wr25 100 70;
    calc_bet_test "calc_bet test wb26 " sr1 w2 wb26 100 70;
    calc_bet_test "calc_bet test wr27 " sr1 w2 wr27 100 70;
    calc_bet_test "calc_bet test wb28 " sr1 w2 wb28 100 70;
    calc_bet_test "calc_bet test wb29 " sr1 w2 wb29 100 70;
    calc_bet_test "calc_bet test wr30 " sr1 w2 wr30 100 70;
    calc_bet_test "calc_bet test wb31 " sr1 w2 wb31 100 70;
    calc_bet_test "calc_bet test wr32 " sr1 w2 wr32 100 70;
    calc_bet_test "calc_bet test wb33 " sr1 w2 wb33 100 70;
    calc_bet_test "calc_bet test wr34 " sr1 w2 wr34 100 70;
    calc_bet_test "calc_bet test wb35 " sr1 w2 wb35 100 70;
    calc_bet_test "calc_bet test wr36 " sr1 w2 wr36 100 70;
    calc_bet_test "calc_bet test wr36 " sr1 w2 wr36 100 70;
    mstf_test "Green 0" "Green 0" sqgs;
    mstf_test "Red 1" "Red 1" sr1;
    mstf_test "Black 2" "Black 2" sb2;
    mstf_test "Red 3" "Red 3" sr3;
    mstf_test "Black 4" "Black 4" sb4;
    mstf_test "Red 5" "Red 5" sr5;
    mstf_test "Black 6" "Black 6" sb6;
    mstf_test "Red 7" "Red 7" sr7;
    mstf_test "Black 8" "Black 8" sb8;
    mstf_test "Red 9" "Red 9" sr9;
    mstf_test "Black 10" "Black 10" sb10;
    mstf_test "Black 11" "Black 11" sb11;
    mstf_test "Red 12" "Red 12" sr12;
    mstf_test "Black 13" "Black 13" sb13;
    mstf_test "Red 14" "Red 14" sr14;
    mstf_test "Black 15" "Black 15" sb15;
    mstf_test "Red 16" "Red 16" sr16;
    mstf_test "Black 17" "Black 17" sb17;
    mstf_test "Red 18" "Red 18" sr18;
    mstf_test "Red 19" "Red 19" sr19;
    mstf_test "Black 20" "Black 20" sb20;
    mstf_test "Red 21" "Red 21" sr21;
    mstf_test "Black 22" "Black 22" sb22;
    mstf_test "Red 23" "Red 23" sr23;
    mstf_test "Black 24" "Black 24" sb24;
    mstf_test "Red 25" "Red 25" sr25;
    mstf_test "Black 26" "Black 26" sb26;
    mstf_test "Red 27" "Red 27" sr27;
    mstf_test "Black 28" "Black 28" sb28;
    mstf_test "Black 29" "Black 29" sb29;
    mstf_test "Red 30" "Red 30" sr30;
    mstf_test "Black 31" "Black 31" sb31;
    mstf_test "Red 32" "Red 32" sr32;
    mstf_test "Black 33" "Black 33" sb33;
    mstf_test "Red 34" "Red 34" sr34;
    mstf_test "Black 35" "Black 35" sb35;
    mstf_test "Red 36" "Red 36" sr36;
    mstf_test "Green Double" "Green Double" sgdbl;
    mstf_test "Color Red" "Color Red" cr;
    mstf_test "Color Black" "Color Black" cb;
    mstf_test "Color Green" "Color Green" cg;
    mstf_test "Dozen One_to_Twelve" "Dozen One_to_Twelve" d1_12;
    mstf_test "Dozen Thirteen_to_Twentyfour" "Dozen Thirteen_to_Twentyfour"
      d13_24;
    mstf_test "Dozen Twentyfive_to_Thirtysix" "Dozen Twentyfive_to_Thirtysix"
      d25_36;
    mstf_test "Half First" "Half First" hf;
    mstf_test "Half Second" "Half Second" hs;
    res_to_string_test "Green Single" wgs "________\n |Green |\n | 0 |\n |__|";
    res_to_string_test "Green Double" wgdbl
      "________\n |Green |\n | 00 | \n |__|";
    res_to_string_test "Red 1" wr1 "________\n |Red |\n | 1 |\n |__|";
    res_to_string_test "Red 3" wr3 "________\n |Red |\n | 3 |\n |__|";
    res_to_string_test "Red 5" wr5 "________\n |Red |\n | 5 |\n |__|";
    res_to_string_test "Red 7" wr7 "________\n |Red |\n | 7 |\n |__|";
    res_to_string_test "Red 9" wr9 "________\n |Red |\n | 9 |\n |__|";
    res_to_string_test "Red 12" wr12 "________\n |Red |\n | 12 |\n |__|";
    res_to_string_test "Red 14" wr14 "________\n |Red |\n | 14 |\n |__|";
    res_to_string_test "Red 16" wr16 "________\n |Red |\n | 16 |\n |__|";
    res_to_string_test "Red 18" wr18 "________\n |Red |\n | 18 |\n |__|";
    res_to_string_test "Red 19" wr19 "________\n |Red |\n | 19 |\n |__|";
    res_to_string_test "Red 21" wr21 "________\n |Red |\n | 21 |\n |__|";
    res_to_string_test "Red 23" wr23 "________\n |Red |\n | 23 |\n |__|";
    res_to_string_test "Red 25" wr25 "________\n |Red |\n | 25 |\n |__|";
    res_to_string_test "Red 27" wr27 "________\n |Red |\n | 27 |\n |__|";
    res_to_string_test "Red 30" wr30 "________\n |Red |\n | 30 |\n |__|";
    res_to_string_test "Red 32" wr32 "________\n |Red |\n | 32 |\n |__|";
    res_to_string_test "Red 34" wr34 "________\n |Red |\n | 34 |\n |__|";
    res_to_string_test "Red 36" wr36 "________\n |Red |\n | 36 |\n |__|";
    res_to_string_test "Black 2" wb2 "________\n |Black |\n | 2 |\n |__|";
    res_to_string_test "Black 4" wb4 "________\n |Black |\n | 4 |\n |__|";
    res_to_string_test "Black 6" wb6 "________\n |Black |\n | 6 |\n |__|";
    res_to_string_test "Black 8" wb8 "________\n |Black |\n | 8 |\n |__|";
    res_to_string_test "Black 10" wb10 "________\n |Black |\n | 10 |\n |__|";
    res_to_string_test "Black 11" wb11 "________\n |Black |\n | 11 |\n |__|";
    res_to_string_test "Black 13" wb13 "________\n |Black |\n | 13 |\n |__|";
    res_to_string_test "Black 15" wb15 "________\n |Black |\n | 15 |\n |__|";
    res_to_string_test "Black 17" wb17 "________\n |Black |\n | 17 |\n |__|";
    res_to_string_test "Black 20" wb20 "________\n |Black |\n | 20 |\n |__|";
    res_to_string_test "Black 22" wb22 "________\n |Black |\n | 22 |\n |__|";
    res_to_string_test "Black 24" wb24 "________\n |Black |\n | 24 |\n |__|";
    res_to_string_test "Black 26" wb26 "________\n |Black |\n | 26 |\n |__|";
    res_to_string_test "Black 28" wb28 "________\n |Black |\n | 28 |\n |__|";
    res_to_string_test "Black 29" wb29 "________\n |Black |\n | 29 |\n |__|";
    res_to_string_test "Black 31" wb31 "________\n |Black |\n | 31 |\n |__|";
    res_to_string_test "Black 33" wb33 "________\n |Black |\n | 33 |\n |__|";
    res_to_string_test "Black 35" wb35 "________\n |Black |\n | 35 |\n |__|"
    (*res_to_string_failtest "Red 36" wb36; res_to_string_failtest "Black 37"
      (Black 37);*);
  ]

let suite = "Test suite for final project" >::: List.flatten [ roul_test ]
let _ = run_test_tt_main suite
