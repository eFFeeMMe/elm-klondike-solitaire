module Commons exposing (..)

import Card
import Klondike.Foundation as Foundation
import Klondike.Tableau as Tableau


c1 : Card.Card
c1 =
    Card.Card Card.Clubs Card.Ace


c2 : Card.Card
c2 =
    Card.Card Card.Clubs Card.Two


c3 : Card.Card
c3 =
    Card.Card Card.Clubs Card.Three


c4 : Card.Card
c4 =
    Card.Card Card.Clubs Card.Four


c5 : Card.Card
c5 =
    Card.Card Card.Clubs Card.Five


c6 : Card.Card
c6 =
    Card.Card Card.Clubs Card.Six


h4 : Card.Card
h4 =
    Card.Card Card.Hearts Card.Four


tableau1 : Tableau.Tableau
tableau1 =
    Tableau.empty
        |> (\tableau -> Tableau.forcePlace tableau [ c5, c4, c3, c2 ])



-- TODO: card fuzzer
-- TODO: card list fuzzer
-- TODO: place n fuzzy cards: is tableau invariant respected?


foundation1 : Foundation.Foundation
foundation1 =
    Foundation.Foundation [ c5, c4, c3 ]
