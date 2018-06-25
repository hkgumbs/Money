module Example exposing (..)

import Expect
import Money
import Test exposing (..)


suite : Test
suite =
    concat
        [ test "dollars can be summed" <|
            \_ ->
                Money.add (Money.dollar 2) (Money.dollar 3)
                    |> Money.reduce exchangeRate Money.USD
                    |> Expect.equal 5
        , test "francs can be summed" <|
            \_ ->
                Money.add (Money.franc 4) (Money.franc 5)
                    |> Money.reduce exchangeRate Money.CHF
                    |> Expect.equal 9
        , test "dollars can combine with francs" <|
            \_ ->
                Money.dollar 5
                    |> Money.add (Money.franc 10)
                    |> Expect.all
                        [ Money.reduce exchangeRate Money.USD
                            >> Expect.equal 10
                        , Money.times 2
                            >> Money.reduce exchangeRate Money.USD
                            >> Expect.equal 20
                        ]
        , test "simple eqaulity accross currencies" <|
            \_ ->
                Money.dollar 20
                    |> Expect.all
                        [ Expect.notEqual (Money.dollar 5)
                        , Expect.equal (Money.dollar 20)
                        , Expect.notEqual (Money.franc 20)
                        ]
        ]


exchangeRate : Money.Currency -> Money.Currency -> Int -> Int
exchangeRate from to amount =
    case ( from, to ) of
        ( Money.USD, Money.USD ) ->
            amount

        ( Money.CHF, Money.CHF ) ->
            amount

        ( Money.USD, Money.CHF ) ->
            amount * 2

        ( Money.CHF, Money.USD ) ->
            amount // 2
