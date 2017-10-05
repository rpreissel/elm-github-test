
import Test exposing (..)
import Test.Runner exposing (..)
import Expect exposing (..)


tests : Test
tests =
    suite "A Test Suite"
        [ test "Addition" (assertEqual (3 + 7) 10)
        , test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
        , test "This test should fail" (assert True)
        ]


main : Program Never
main =
    runSuiteHtml tests
