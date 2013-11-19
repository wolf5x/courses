fib_recursively := method(n,
    n type println
    if ((n type != "Number") or (n <= 0),
        0,
        if (n > 2, fib_recursively(n-1) + fib_recursively(n-2), 1)
    )
)

fib_iteratively := method(n,
    if ((n type != "Number") or (n < 0), 
        0,
        x := 1; y := 1
        (n-2) repeat(t := x; x = y; y = t+x)
        y
    )
)

