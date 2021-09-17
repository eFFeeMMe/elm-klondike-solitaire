module ListExtra exposing (listExtra)

import Expect
import List.Extra
import Test exposing (..)


{-| This is just here as a reminder of how to use List.Extra functions
-}
listExtra : Test
listExtra =
    describe "List.Extra functions"
        [ test "I needed an example to see how List.Extra.splitWhen works :<" <|
            \_ ->
                Expect.equal
                    (List.Extra.splitWhen ((==) 3) [ 2, 3, 4 ])
                    (Just ( [ 2 ], [ 3, 4 ] ))
        , test "I needed an example to see how List.Extra.splitAt works :<" <|
            \_ ->
                Expect.equal
                    (List.Extra.splitAt 2 [ 2, 3, 4 ])
                    ( [ 2, 3 ], [ 4 ] )
        ]
