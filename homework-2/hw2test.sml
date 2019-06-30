(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2.sml";

val test1_1 = all_except_option ("string", ["string"]) = SOME []
val test1_2 = all_except_option ("string", []) = NONE
val test1_3 = all_except_option ("string", ["str", "string", "str2"]) = SOME ["str", "str2"]
val test1_4 = all_except_option ("string", ["x", "y", "z"]) = NONE

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3_1 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test3_2 = get_substitutions2 ([["foo","bar"],["there"],["ahh"]], "foo") = ["bar"]

val test3_3 = get_substitutions2 ([["foo","bar","foo"],["there","foo"]], "foo") = ["bar","there"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)

val test14_2 = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace),(Hearts,Ace)],
                                    [Draw,Draw,Draw,Draw,Draw],
                                    42)
               = 7

val test14_3 = officiate_challenge ([(Clubs,Ace),(Spades,Ace)],
                                    [Draw,Draw,Draw,Draw,Draw],
                                    3)
               = 0

val test15_1 = careful_player ([(Clubs,Ace),(Spades,Num 1)],
                               12)
               = [Draw, Draw]

val test15_2 = careful_player ([(Clubs,Ace),(Spades,Num 1)],
                               14)
               = [Draw, Draw]

val test15_3 = careful_player ([(Clubs,Ace),(Spades,Num 1)],
                               23)
               = [Draw, Draw, Draw]

val test15_4 = careful_player ([(Clubs,Ace),(Spades,Num 3)],
                               12)
               = [Draw, Discard (Clubs,Ace), Draw]

val test15_5 = careful_player ([(Clubs,Ace),(Spades,Num 2),(Hearts,Num 4),(Diamonds,Num 1)],
                               15)
               = [Draw, Draw, Discard (Spades,Num 2), Draw]

val test15_6 = careful_player ([(Clubs,Ace),(Spades,Num 2),(Clubs,Num 3),(Diamonds,Num 1)],
                               15)
               = [Draw, Draw, Discard (Spades,Num 2), Draw]

val test15_7 = careful_player ([(Clubs,Ace),(Spades,Num 2),(Hearts,Num 3),(Diamonds,Num 1),(Clubs,King)],
                               15)
               = [Draw, Draw, Discard (Spades,Num 2), Draw, Draw]

val test15_8 = careful_player ([],
                               15)
               = [Draw]

val test15_9 = careful_player ([(Clubs,Ace),(Spades,Num 2)],
                               1)
               = []
