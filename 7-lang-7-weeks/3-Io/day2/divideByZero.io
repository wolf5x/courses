Number origDiv := Number getSlot("/")
Number / := method(n,
    if(n != 0, self origDiv(n), 0)
)

