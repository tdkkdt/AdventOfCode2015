module AOC2015.Checker

let check (expected : 'T) (actual : 'T) =
    if (expected <> actual) then
        failwithf "Expected %O but was %O\r\n" expected actual