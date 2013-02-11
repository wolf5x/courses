val s1 = ["Abdfdh", "dbksdf", "i", "a", "bDFB", "BdXXyz"]
val test1 = only_capitals(s1)
val test2 = longest_string1(s1)
val test3 = longest_string2(s1)
val test4 = longest_string3(s1)
val test4_2 = longest_string4(s1)


val test5 = longest_capitalized(["Abdfd", "dbksdf", "i", "A", "bDFB",
"BdXX", "Caaaa"])

val test6 = rev_string "1234567890"

val test8 = all_answers (fn Wildcard => SOME [1] | _ => SOME[0]) ([Variable "x",
Wildcard, ConstructorP("y",UnitP)])

val test8_2 = all_answers (fn Tuple x => SOME x) []

val pat9 = (ConstructorP("s",TupleP([Wildcard, Variable("uvw"), UnitP, ConstP(99), Wildcard, TupleP([Variable("uvw"),Variable("xyx"),ConstructorP("s2",Wildcard)])])))

val test9a = count_wildcards pat9
val test9b = count_wild_and_variable_lengths pat9
val test9c = count_some_var ("uvw",pat9)

val pat10 = (ConstructorP("s",TupleP([Wildcard, Variable("uvw"), UnitP, ConstP(99), Wildcard, TupleP([Variable("abc"),Variable("xyx"),ConstructorP("s2",Wildcard)])])))
val test10 = check_pat(pat9)
val test10_2 = check_pat(pat10)

val val11 = (Constructor("s", Tuple([Const(11), Tuple([]), Unit, Const(99),
Unit, Tuple([Const(22), Constructor("xx", Unit), Constructor("s2", Unit)])])))
val val11_2 = (Constructor("s", Tuple([Const(11), Tuple([]), Unit, Const(99),
Unit, Tuple([Const(22), Constructor("xx", Unit), Constructor("s3", Unit)])])))
val val11_3 = (Constructor("s", Tuple([Const(11), Tuple([]), Unit, Const(77),
Unit, Tuple([Const(22), Constructor("xx", Unit), Constructor("s2", Unit)])])))
val test11 = match(val11, pat10)
val test11_2 = match(val11_2, pat10)
val test11_3 = match(val11_3, pat10)

val val12 = Tuple[Unit, Constructor("ss",Const(66))]
val pat12 = [UnitP,
TupleP[UnitP,ConstructorP("ss",ConstP(55))],TupleP[Variable("tt"),ConstructorP("ss",ConstP(66))]]
val test12 = first_match val12 pat12

