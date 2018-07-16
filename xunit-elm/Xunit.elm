module Xunit exposing (..)


type Test
    = Individual (() -> Bool)
    | Suite (List Test)


type alias TestResult =
    { passCount : Int
    , errorCount : Int
    }


suite : List Test -> Test
suite =
    Suite


run : Test -> String
run test =
    summary <| runHelp test (TestResult 0 0)


runHelp : Test -> TestResult -> TestResult
runHelp test result =
    case test of
        Suite subTests ->
            List.foldl runHelp result subTests

        Individual method ->
            if method () then
                { result | passCount = result.passCount + 1 }
            else
                { result | errorCount = result.errorCount + 1 }


summary : TestResult -> String
summary { passCount, errorCount } =
    toString (passCount + errorCount)
        ++ " run, "
        ++ toString errorCount
        ++ " failed"



-- META TEST TESTER


goodTest : Test
goodTest =
    Individual <| \_ -> True


badTest : Test
badTest =
    Individual <| \_ -> False


main : Program Never () ()
main =
    let
        _ =
            Debug.log ""
                [ run goodTest == "1 run, 0 failed"
                , run badTest == "1 run, 1 failed"
                , run (suite [ goodTest, badTest ]) == "2 run, 1 failed"
                ]
    in
    Platform.program
        { init = ( (), Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = always Sub.none
        }
