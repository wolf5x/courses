Matrix := Object clone

Matrix dim := method(x, y,
    m := self clone
    m container := List clone setSize(x)
    for(i, 0, x-1,
        m container atPut(i, List clone setSize(y))
        for(j, 0, y-1, m set(i, j, 0))
    )
    m
)

Matrix set := method(x, y, v,
    self container at(x) atPut(y, v)
    self
)

Matrix get := method(x, y,
    self container at(x) at(y)
)

Matrix rowSize := method(
    self container size
)

Matrix columnSize := method(
    if(rowSize > 0, self container at(0) size, 0)
)

Matrix transpose := method(
    r := rowSize
    c := columnSize
    m := Matrix clone dim(c, r)
    for(i, 0, c-1,
        for(j, 0, r-1,
            m set(i,j, self get(j,i))
        )
    )
    m
)

Matrix saveToFile := method(aPath,
    f := File with(aPath) openForUpdating
    r := rowSize
    c := columnSize
    f write(r asString, " ", c asString, "\n")
    for(i, 0, r-1,
        for(j, 0, c-1,
            f write(get(i,j) asString, if(j<c-1, " ", "\n"))
        )
    )
    f close
    self
)

Matrix loadFromFile := method(aPath,
    buf := File with(aPath) asBuffer split map(v, v asNumber)
    r := buf removeFirst
    c := buf removeFirst
    m := Matrix dim(r,c)
    for(i, 0, r-1,
        for(j, 0, c-1,
            m set(i, j, buf removeFirst)
        )
    )
    m
)

m := Matrix dim(3,4) set(0,1,1) set(1,2,99999) saveToFile("matrix.txt")
m := nil
n := Matrix loadFromFile("matrix.txt")


