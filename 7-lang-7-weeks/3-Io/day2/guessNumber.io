Guess := Object clone

Guess init := method(
    args := call message arguments
    self triesLeft := 10
    self lower := 1
    self upper := 100
    self answer := Random value(lower, upper) floor
    if(args size == 1, 
        self triesLeft := argsAt(0),
        if(args size == 3,
            self lower := argsAt(1)
            self upper := argsAt(2)
        )
    )
    self
)

Guess doGuess := method(x,



