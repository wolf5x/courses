List myAverage := method(
    if(size > 0, sum / size, 0)
)

List myAverage2 := method(
    if (self size > 0, 
        (self reduce(acc, x, acc + if(x type == "Number", x, Exception clone), 0)) / (self size), 
        0
    )
)

    
