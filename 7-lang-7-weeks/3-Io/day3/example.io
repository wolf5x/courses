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
Builder indentLevel := 0
Builder forward = method(
    write(makeIndent() .. "<", call message name)
    indentLevel = indentLevel + 1
    isFirst := true
    call message arguments foreach(
        arg,
        if(isFirst,
            if(arg name == "curlyBrackets", 
                (self doMessage(arg)) printAsAttributes
            )

            write(">\n")
            isFirst = false
        )

        content := self doMessage(arg)
        if(content type == "Sequence", writeln(makeIndent() .. content))
    )
    indentLevel = indentLevel - 1
    writeln(makeIndent() .. "</", call message name, ">")
)

Builder makeIndent := method(
    spaces := ""
    indentLevel repeat(spaces = spaces .. "  ")
    return spaces
)


Builder ui({"att":"rib"})

