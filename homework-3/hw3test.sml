(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10_1 = check_pat (Variable("x")) = true

val test10_2 = check_pat (ConstructorP ("hi",TupleP[Variable "x",Variable "x"])) = false

val test10_3 = check_pat (ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false

val test11_1 = match (Const(1), UnitP) = NONE

val test11_2 = match (Unit, UnitP) = SOME []

val test11_3 = match (Tuple([Const(1), Unit]), Wildcard) = SOME []

val test11_4 = match (Tuple([Const(1), Unit]), Variable("v")) = SOME [("v", Tuple([Const(1), Unit]))]

val test11_5 = match (Tuple([Const(1), Constructor("foo", Const(2))]), TupleP([Variable("x"), ConstructorP("foo", Variable("y"))])) = SOME [("x", Const(1)), ("y", Const(2))]

val test11_6 = match (Tuple([Const(1), Constructor("foo", Const(2))]), TupleP([Variable("x"), ConstructorP("bar", Variable("y"))])) = NONE

val test11_7 = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],TupleP[Wildcard,Wildcard]) = NONE

val test12_1 = first_match Unit [UnitP] = SOME []

val test12_2 = first_match (Tuple([Const(1), Constructor("foo", Const(2))])) [Wildcard, TupleP([Variable("x"), ConstructorP("foo", Variable("y"))])] = SOME []

val test12_3 = first_match (Tuple([Const(1), Constructor("foo", Const(2))])) [TupleP([Variable("x"), ConstructorP("foo", Variable("y"))]), Wildcard] = SOME [("x", Const(1)), ("y", Const(2))]

(* challenge problem *)

val test13_1 = typecheck_patterns([], [TupleP[Variable("x"),Variable("y")], TupleP[Wildcard,Wildcard]]) = SOME (TupleT[Anything,Anything])

val test13_2 = typecheck_patterns([], [TupleP[Wildcard,Wildcard], TupleP[Wildcard,TupleP[Wildcard,Wildcard]]]) = SOME (TupleT[Anything,TupleT[Anything,Anything]])

val test13_3 = typecheck_patterns([], [TupleP[Wildcard,TupleP[ConstP(16),Wildcard]], TupleP[Wildcard,TupleP[Wildcard,Wildcard]]]) = SOME (TupleT[Anything,TupleT[IntT,Anything]])

val test13_4 = typecheck_patterns([], [TupleP[Wildcard,TupleP[ConstP(16),Wildcard]], TupleP[Wildcard,TupleP[UnitP,Wildcard]]]) = NONE

val test13_5 = typecheck_patterns([("bar", "out", UnitT), ("foo", "out", IntT)], [ConstructorP("foo", ConstP(2)), ConstructorP("bar", UnitP)]) = SOME (Datatype("out"))

val test13_6 = typecheck_patterns([("bar", "out", UnitT), ("foo", "blah", IntT)], [ConstructorP("foo", ConstP(3)), ConstructorP("bar", UnitP)]) = NONE
