> items rolled up on both sides of a focus

## 📜 [`Scroll`](https://package.elm-lang.org/packages/lue-bird/elm-scroll/latest/)


→ good fit for dynamic choice selection: tabs, playlist, ...
[↑ examples](https://github.com/lue-bird/elm-emptiness-typed/tree/master/examples)

`Scroll` can even focus a gap
[`Down` or `Up`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)
from every item


```elm
import Linear exposing (Direction(..))
import Emptiable exposing (filled)
import Stack exposing (topBelow)
import Scroll exposing (Scroll, FocusGap)

Scroll.empty
    --: Scroll item_ FocusGap Possibly
    |> Scroll.focusAlter (\_ -> -1 |> filled)
    --: Scroll number_ FocusGap never_
    |> Scroll.sideAlter Up
        (\_ -> topBelow 1 [ 2, 3 ])
    --: Scroll number_ FocusGap never_
    |> Scroll.toGap Up
    --: Scroll number_ FocusGap Possibly
    |> Scroll.focusAlter (\_ -> 0 |> filled)
    --: Scroll number_ FocusGap never_
    |> Scroll.toStack
--> topBelow -1 [ 0, 1, 2, 3 ]
```

## suggestions?

→ See [contributing.md](https://github.com/lue-bird/elm-scroll/blob/master/contributing.md)

