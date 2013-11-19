OperatorTable addAssignOperator(":", "atPutNumber")

Map atPutNumber := method(
    self atPut(
        call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),
        call evalArgAt(1)
    )
)

curlyBrackets := method(
    r := Map clone
    call message arguments foreach(arg, 
        r doMessage(arg)
    )
    r
)

Map printAsAttributes := method(
    self foreach(k, v,
        write(" " .. k .. "=\"" .. v .. "\"")
    )
)

Builder := Object clone
Builder indentPos := 0

Builder forward := method(
    write(makeIndent(), "<", call message name)
    indentPos = indentPos+1
    isFirst := true
    call message arguments foreach(
        arg,
        if(isFirst, 
            if(arg name == "curlyBrackets",
                (self doMessage(arg)) printAsAttributes
            )
            writeln(">")
            isFirst = false
        )
        content := self doMessage(arg)
        if(content type == "Sequence", writeln(makeIndent(), content))
    )
    indentPos = indentPos-1
    writeln(makeIndent(), "</", call message name, ">")
)

Builder makeIndent := method(
    ws := ""
    indentPos repeat(ws = ws .. "  ")
    return ws
)

Builder ui({"depth" : 1, "type" : "myui"}, li({"goodattr" : "me"}, "abdc"), li("def", {"illegal" : "yes"}), div({"nochild" : true}))
    
