MyObject := Object clone
MyObject forward := method(
    write("sender = ", call sender, "\n")
    write("message name = ", call message name, "\n")
    args := call message argsEvaluatedIn(call sender)
    args foreach(i, v, write("arg", i, " = ", v, "\n"))
)

