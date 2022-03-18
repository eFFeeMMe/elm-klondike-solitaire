module Foundation exposing (place)

import Commons exposing (c1, c6, foundation1, h4)
import Expect
import Klondike.Foundation as Foundation
import Test


place : Test.Test
place =
    Test.describe "Foundation.place"
        [ Test.test "should be able to put an ace on an empty foundation" <|
            \_ ->
                Foundation.place Foundation.empty c1
                    |> Expect.equal
                        (Just (Foundation.forcePlace Foundation.empty c1))
        , Test.test "should return Nothing when the move is illegal" <|
            \_ ->
                Foundation.place foundation1 h4
                    |> Expect.equal Nothing
        , Test.test "should return the Foundation with its new card when the move is legal" <|
            \_ ->
                Foundation.place foundation1 c6
                    |> Expect.equal
                        (Just (Foundation.forcePlace foundation1 c6))
        ]
