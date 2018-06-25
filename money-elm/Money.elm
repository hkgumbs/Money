module Money exposing (Currency(..), Money, add, dollar, franc, reduce, times)


type Money
    = Money Currency Int
    | Add Money Money
    | Times Int Money


type Currency
    = USD
    | CHF


dollar : Int -> Money
dollar =
    Money USD


franc : Int -> Money
franc =
    Money CHF


reduce : (Currency -> Currency -> Int -> Int) -> Currency -> Money -> Int
reduce exchangeRate target money =
    case money of
        Money currency amount ->
            exchangeRate currency target amount

        Add augend addend ->
            reduce exchangeRate target augend + reduce exchangeRate target addend

        Times multiplier multiplicand ->
            multiplier * reduce exchangeRate target multiplicand



-- ARITHMETIC


add : Money -> Money -> Money
add =
    Add


times : Int -> Money -> Money
times =
    Times
