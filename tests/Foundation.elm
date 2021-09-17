module Foundation exposing (place)

import Commons exposing (c1, c2, c3, c4, c5, c6, foundation1, h4)
import Expect
import Klondike.Foundation as Foundation exposing (Foundation(..))
import Test exposing (..)


emptyFoundation =
    Foundation []


place : Test
place =
    describe "Foundation.place"
        [ test "should be able to put an ace on an empty foundation" <|
            \_ ->
                Foundation.place emptyFoundation c1
                    |> Expect.equal
                        (Just (Foundation.forcePlace emptyFoundation c1))
        , test "should return Nothing when the move is illegal" <|
            \_ ->
                Foundation.place foundation1 h4
                    |> Expect.equal Nothing
        , test "should return the Foundation with its new card when the move is legal" <|
            \_ ->
                Foundation.place foundation1 c6
                    |> Expect.equal
                        (Just (Foundation.forcePlace foundation1 c6))
        ]
