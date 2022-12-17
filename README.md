> items rolled up on both sides of a focus

## 📜 [`Scroll`](https://package.elm-lang.org/packages/lue-bird/elm-scroll/latest/)


→ good fit for dynamic choice selection: tabs, playlist, ...
[↑ examples](https://github.com/lue-bird/elm-emptiness-typed/tree/master/example)

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

## alternatives

  - [zwilias/elm-holey-zipper](https://package.elm-lang.org/packages/zwilias/elm-holey-zipper/latest).
    unsafe; a bit cluttered; no `map (Location -> ...)`, `dragFocus`, `toNonEmptyList`, `sideAlter` (so no squeezing in multiple items, ...)
  - [turboMaCk/non-empty-list-alias: `List.NonEmpty.Zipper`](https://dark.elm.dmy.fr/packages/turboMaCk/non-empty-list-alias/latest/List-NonEmpty-Zipper)
    complete; cluttered (for example `update` & `map`); some unintuitive names
  - [miyamoen/select-list](https://dark.elm.dmy.fr/packages/miyamoen/select-list/latest/SelectList)
    complete; a bit cluttered; no `focusWhere`
  - [yotamDvir/elm-pivot](https://dark.elm.dmy.fr/packages/yotamDvir/elm-pivot/latest/)
    complete; cluttered; no `map (Location -> ...)`
  - [STTR13/ziplist](https://dark.elm.dmy.fr/packages/STTR13/ziplist/latest/)
    navigation works; a bit cluttered; no `map (Location -> ...)`, `sideAlter` (so no squeezing in multiple items, ...)
  - [wernerdegroot/listzipper](https://dark.elm.dmy.fr/packages/wernerdegroot/listzipper/latest/List-Zipper)
    navigation works; no `dragFocus`, `map (Location -> ...)`, `toNonEmptyList`
  - [alexanderkiel/list-selection](https://dark.elm.dmy.fr/packages/alexanderkiel/list-selection/latest/List-Selection)
    & [NoRedInk/list-selection](https://dark.elm.dmy.fr/packages/NoRedInk/list-selection/latest/List-Selection)
    very incomplete, impossible to extract focused item safely; no navigation, insertion, `side`, ...
  - [jjant/elm-comonad-zipper](https://dark.elm.dmy.fr/packages/jjant/elm-comonad-zipper/latest/)
    incomplete; no `dragFocus`, `toNonEmptyList`, `sideAlter` (so no squeezing in multiple items, ...)
  - [guid75/ziplist](https://dark.elm.dmy.fr/packages/guid75/ziplist/latest/)
    extremely incomplete
  - [arowM/elm-reference: `Reference.List`](https://dark.elm.dmy.fr/packages/arowM/elm-reference/latest/Reference-List)
    only `overList`, sides impossible to access & alter
