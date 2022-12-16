module Scroll exposing
    ( Scroll(..), FocusGap(..)
    , Location(..), nearest
    , empty, one
    , focusItem, focus
    , side
    , length
    , to, toGap
    , toEnd, toEndGap
    , toWhere
    , focusDrag
    , mirror
    , focusAlter, sideAlter
    , map, focusSidesMap
    , focusItemTry
    , foldFrom, fold, foldFromOne
    , toStack, toList
    , focusGapAdapt
    )

{-| 📜 Items rolled up on both sides of a focus

→ good fit for dynamic choice selection: tabs, playlist, timeline...
[↑ examples](https://github.com/lue-bird/elm-emptiness-typed/tree/master/example)

[`Scroll`](#Scroll) can even focus a gap
[`Down` or `Up`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)
from every item

Not what you were looking for? Check out [alternatives](#alternatives)

@docs Scroll, FocusGap


## position

@docs Location, nearest


## create

@docs empty, one


## scan

@docs focusItem, focus
@docs side
@docs length


## move the focus

@docs to, toGap
@docs toEnd, toEndGap
@docs toWhere
@docs focusDrag


### alter

@docs mirror
@docs focusAlter, sideAlter
@docs map, focusSidesMap
@docs focusItemTry


## transform

@docs foldFrom, fold, foldFromOne
@docs toStack, toList


## type-level

@docs focusGapAdapt


## alternatives

  - [zwilias/elm-holey-zipper](https://package.elm-lang.org/packages/zwilias/elm-holey-zipper/latest).
    unsafe; a bit cluttered; no `map (Location -> ...)`, `focusDrag`, `toNonEmptyList`, `sideAlter` (so no squeezing in multiple items, ...)
  - [turboMaCk/non-empty-list-alias: `List.NonEmpty.Zipper`](https://dark.elm.dmy.fr/packages/turboMaCk/non-empty-list-alias/latest/List-NonEmpty-Zipper)
    complete; cluttered (for example `update` & `map`); some unintuitive names
  - [miyamoen/select-list](https://dark.elm.dmy.fr/packages/miyamoen/select-list/latest/SelectList)
    complete; a bit cluttered; no `focusWhere`
  - [yotamDvir/elm-pivot](https://dark.elm.dmy.fr/packages/yotamDvir/elm-pivot/latest/)
    complete; cluttered; no `map (Location -> ...)`
  - [STTR13/ziplist](https://dark.elm.dmy.fr/packages/STTR13/ziplist/latest/)
    navigation works; a bit cluttered; no `map (Location -> ...)`, `sideAlter` (so no squeezing in multiple items, ...)
  - [wernerdegroot/listzipper](https://dark.elm.dmy.fr/packages/wernerdegroot/listzipper/latest/List-Zipper)
    navigation works; no `focusDrag`, `map (Location -> ...)`, `toNonEmptyList`
  - [alexanderkiel/list-selection](https://dark.elm.dmy.fr/packages/alexanderkiel/list-selection/latest/List-Selection)
    & [NoRedInk/list-selection](https://dark.elm.dmy.fr/packages/NoRedInk/list-selection/latest/List-Selection)
    very incomplete, impossible to extract focused item safely; no navigation, insertion, `side`, ...
  - [jjant/elm-comonad-zipper](https://dark.elm.dmy.fr/packages/jjant/elm-comonad-zipper/latest/)
    incomplete; no `focusDrag`, `toNonEmptyList`, `sideAlter` (so no squeezing in multiple items, ...)
  - [guid75/ziplist](https://dark.elm.dmy.fr/packages/guid75/ziplist/latest/)
    extremely incomplete
  - [arowM/elm-reference: `Reference.List`](https://dark.elm.dmy.fr/packages/arowM/elm-reference/latest/Reference-List)
    only `overList`, sides impossible to access & alter

-}

import Emptiable exposing (Emptiable(..), emptyAdapt, fill, filled)
import Linear exposing (Direction(..))
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked, onTopLay, removeTop, top)


{-| 📜 Items rolled up on both sides of a focus

→ good fit for dynamic choice selection: tabs, playlist, ...

`Scroll` can even focus a gap `Down` and `Up` every item:

  - `🍍 🍓 <🍊> 🍉 🍇`: `Scroll ... FocusGap Never`

  - `🍍 🍓 <?> 🍉 🍇`: `Scroll ... FocusGap` [`Possibly`](https://dark.elm.dmy.fr/packages/lue-bird/elm-allowable-state/latest/Possibly)

    `<?>` means both are possible:

      - `🍍 🍓 <> 🍉 🍇`: a gap between items ... heh
      - `🍍 🍓 <🍊> 🍉 🍇`


#### in arguments

    empty : Scroll item_ FocusGap Possibly


#### in types

    type alias Model =
        RecordWithoutConstructorFunction
            { choice : Scroll Option FocusGap Never
            }

where [`RecordWithoutConstructorFunction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/)
stops the compiler from creating a constructor function for `Model`

-}
type Scroll item focusGapTag possiblyOrNever
    = BeforeFocusAfter
        (Emptiable (Stacked item) Possibly)
        (Emptiable item possiblyOrNever)
        (Emptiable
            (Stacked item)
            Possibly
        )


{-| A word in every [`Scroll`](#Scroll) type:

  - `🍍 🍓 <🍊> 🍉 🍇`: `Scroll ... FocusGap Never`

  - `🍍 🍓 <?> 🍉 🍇`: `Scroll ... FocusGap` [`Possibly`](https://dark.elm.dmy.fr/packages/lue-bird/elm-allowable-state/latest/Possibly)

    `<?>` means both are possible:

      - `🍍 🍓 <> 🍉 🍇`: a gap between items ... Heh.
      - `🍍 🍓 <🍊> 🍉 🍇`

-}
type FocusGap
    = FocusGap Never



-- position


{-| Position in a [`Scroll`](#Scroll) relative to its focus

    import Linear exposing (Direction(..))
    import Emptiable exposing (Emptiable, filled)
    import Stack exposing (topBelow)

    Scroll.one 0
        |> Scroll.sideAlter Up
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.to (Scroll.AtSide Up 2)
        |> Emptiable.map Scroll.focusItem
    --> filled 3
    --: Emptiable (Stacked number_) Possibly

-}
type Location
    = AtSide Linear.Direction Int
    | AtFocus


{-| The [`Location`](#Location) directly
[`Down`|`Up`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/) the focus

    import Emptiable exposing (Emptiable, filled)
    import Stack exposing (onTopLay, topBelow)
    import Scroll exposing (Scroll, FocusGap)
    import Linear exposing (Direction(..))

    Scroll.one "hello"
        |> Scroll.sideAlter Up
            (\_ -> topBelow "scrollable" [ "world" ])
        |> Scroll.toEnd Up
        |> Scroll.to (Down |> Scroll.nearest)
        |> Emptiable.map Scroll.focusItem
    --> filled "scrollable"
    --: Emptiable (Scroll String FocusGap Never) Possibly

    Scroll.empty
        |> Scroll.sideAlter Down
            (\_ -> topBelow "world" [ "scrollable" ])
        |> Scroll.to (Down |> Scroll.nearest)
    --> Scroll.one "world"
    -->     |> Scroll.sideAlter Down
    -->         (\_ -> Stack.one "scrollable")
    -->     |> filled
    --: Emptiable (Scroll String FocusGap Never) Possibly

    Scroll.empty
        |> Scroll.sideAlter Up
            (onTopLay "foo")
        |> Scroll.to (Up |> Scroll.nearest)
    --> filled (Scroll.one "foo")
    --: Emptiable (Scroll String FocusGap Never) Possibly

    nearest =
        \side ->
            Scroll.AtSide side 0

-}
nearest : Linear.Direction -> Location
nearest =
    \side_ ->
        AtSide side_ 0



--


{-| An empty `Scroll` on a gap
with nothing before and after it.
It's the loneliest of all [`Scroll`](#Scroll)s

```monospace
<>
```

    import Emptiable

    Scroll.empty |> Scroll.toStack
    --> Emptiable.empty

-}
empty : Scroll item_ FocusGap Possibly
empty =
    BeforeFocusAfter Emptiable.empty Emptiable.empty Emptiable.empty


{-| A `Scroll` with a single focussed item in it,
nothing `Down` and `Up` it

```monospace
🍊  ->  <🍊>
```

    import Stack

    Scroll.one "wat" |> Scroll.focusItem
    --> "wat"

    Scroll.one "wat" |> Scroll.toStack
    --> Stack.one "wat"

-}
one : element -> Scroll element FocusGap never_
one currentItem =
    BeforeFocusAfter Emptiable.empty (filled currentItem) Emptiable.empty



--


{-| The focused item

```monospace
🍍 🍓 <🍊> 🍉 🍇  ->  🍊
```

    import Stack exposing (topBelow)
    import Linear exposing (Direction(..))

    Scroll.one "hi there" |> Scroll.focusItem
    --> "hi there"

    Scroll.one 1
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ 3, 4 ])
        |> Scroll.toEnd Up
        |> Scroll.focusItem
    --> 4

-}
focusItem : Scroll item FocusGap Never -> item
focusItem =
    \scroll -> scroll |> focus |> Emptiable.fill


{-| The focused item or gap

```monospace
🍍 🍓 <🍊> 🍉 🍇  ->  🍊
🍍 🍓 <> 🍉 🍇  ->  _
```

    import Emptiable exposing (filled, fill)

    Scroll.empty |> Scroll.focus
    --> Emptiable.empty

    Scroll.one "hi there" |> Scroll.focus |> fill
    --> "hi there"

[`focusItem`](#focusItem) is short for `focus |> fill`

-}
focus :
    Scroll item FocusGap possiblyOrNever
    -> Emptiable item possiblyOrNever
focus =
    \(BeforeFocusAfter _ focus_ _) -> focus_


{-| The [`Stack`](Stack) to one [side](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/) of the focus

`Down`

```monospace
🍍←🍓) <🍊> 🍉 🍇
```

`Up`

```monospace
🍍 🍓 <🍊> (🍉→🍇
```

    import Emptiable exposing (Emptiable)
    import Stack exposing (Stacked, topBelow)
    import Linear exposing (Direction(..))

    Scroll.one 0
        |> Scroll.sideAlter Up
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.to (Up |> Scroll.nearest)
        |> Emptiable.mapFlat (Scroll.to (Up |> Scroll.nearest))
        |> Emptiable.mapFlat (Scroll.side Down)
    --> topBelow 1 [ 0 ]
    --: Emptiable (Stacked number_) Possibly

    Scroll.one 0
        |> Scroll.sideAlter Up
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.to (Up |> Scroll.nearest)
        |> Emptiable.mapFlat (Scroll.side Up)
    --> topBelow 2 [ 3 ]
    --: Emptiable (Stacked number_) Possibly

-}
side :
    Linear.Direction
    ->
        (Scroll item FocusGap possiblyOrNever_
         -> Emptiable (Stacked item) Possibly
        )
side sideToAccess =
    \scroll ->
        let
            (BeforeFocusAfter sideBefore _ sideAfter) =
                scroll
        in
        case sideToAccess of
            Down ->
                sideBefore

            Up ->
                sideAfter


{-| Counting all contained items

    import Stack exposing (topBelow)
    import Linear exposing (Direction(..))

    Scroll.one 0
        |> Scroll.sideAlter Down
            (\_ -> topBelow -1 [ -2 ])
        |> Scroll.sideAlter Up
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.length
    --> 6

    Scroll.empty
        |> Scroll.sideAlter Down
            (\_ -> topBelow -1 [ -2 ])
        |> Scroll.sideAlter Up
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.length
    --> 5

`O(n)` like [`Scroll.length`](Scroll#length)

-}
length : Scroll item_ FocusGap possiblyOrNever_ -> Int
length =
    \(BeforeFocusAfter before focus_ after) ->
        (before |> Stack.length)
            + (case focus_ of
                Filled _ ->
                    1

                Empty _ ->
                    0
              )
            + (after |> Stack.length)



--


{-| Try to move the focus to the nearest item [`Down|Up`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)

**Should not be exposed**

-}
toItemNearest :
    Linear.Direction
    ->
        (Scroll item FocusGap possiblyOrNever_
         -> Emptiable (Scroll item FocusGap never_) Possibly
        )
toItemNearest side_ =
    \scroll ->
        (scroll |> side side_)
            |> Emptiable.map
                (\stacked ->
                    let
                        sideNew =
                            stacked |> filled |> removeTop

                        focusNew =
                            stacked |> filled |> top |> filled

                        sideOppositeNew =
                            (scroll |> side (side_ |> Linear.opposite))
                                |> Stack.attach Down
                                    (scroll |> focus |> Emptiable.mapFlat Stack.one)
                    in
                    case side_ of
                        Down ->
                            BeforeFocusAfter sideNew focusNew sideOppositeNew

                        Up ->
                            BeforeFocusAfter sideOppositeNew focusNew sideNew
                )


{-| Try to move the [`focus`](#focus) to the item at a given [`Scroll.Location`](#Location)


#### `Scroll.to (Down |> Scroll.nearest)`

```monospace
🍊 <🍉> 🍇  ->  <🍊> 🍉 🍇
```

    import Linear exposing (Direction(..))
    import Emptiable exposing (Emptiable, filled)
    import Scroll exposing (Scroll, FocusGap)

    Scroll.empty |> Scroll.to (Down |> Scroll.nearest)
    --> Emptiable.empty

    Scroll.one "hello"
        |> Scroll.sideAlter Up
            (\_ -> topBelow "scrollable" [ "world" ])
        |> Scroll.toEnd Up
        |> Scroll.to (Down |> Scroll.nearest)
        |> Emptiable.map Scroll.focusItem
    --> filled "scrollable"
    --: Emptiable (Scroll String FocusGap Never) Possibly

This also works from within gaps:

```monospace
🍊 🍉 <> 🍇  ->  🍊 <🍉> 🍇
```

    import Linear exposing (Direction(..))
    import Emptiable exposing (Emptiable, filled)
    import Stack exposing (onTopLay)
    import Scroll exposing (Scroll, FocusGap)

    Scroll.empty
        |> Scroll.sideAlter Down
            (onTopLay "foo")
        |> Scroll.to (Down |> Scroll.nearest)
    --> filled (Scroll.one "foo")
    --: Emptiable (Scroll String FocusGap Never) Possibly


#### `Scroll.to (Up |> Scroll.nearest)`

```monospace
<🍊> 🍉 🍇  ->  🍊 <🍉> 🍇
```

    import Linear exposing (Direction(..))
    import Emptiable exposing (Emptiable, filled)
    import Scroll exposing (Scroll, FocusGap)

    Scroll.one 0
        |> Scroll.sideAlter Up
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.to (Up |> Scroll.nearest)
        |> Emptiable.map Scroll.focusItem
    --> filled 1
    --: Emptiable number_ Possibly

This also works from within gaps:

```monospace
🍊 <> 🍉 🍇  ->  🍊 <🍉> 🍇
```

    import Linear exposing (Direction(..))
    import Emptiable exposing (Emptiable, filled)
    import Stack exposing (onTopLay)
    import Scroll exposing (Scroll, FocusGap)

    Scroll.empty
        |> Scroll.sideAlter Up
            (\_ -> Stack.one "foo")
        |> Scroll.to (Up |> Scroll.nearest)
    --> filled (Scroll.one "foo")
    --: Emptiable (Scroll String FocusGap Never) Possibly

If there is no next item, the result is [`empty`](Emptiable#empty)

    import Linear exposing (Direction(..))
    import Emptiable
    import Stack exposing (topBelow)

    Scroll.empty |> Scroll.to (Up |> Scroll.nearest)
    --> Emptiable.empty

    Scroll.one 0
        |> Scroll.sideAlter Up
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.toEnd Up
        |> Scroll.to (Up |> Scroll.nearest)
    --> Emptiable.empty


#### `Scroll.to Location`

    import Element as Ui
    import Element.Input as UIn
    import Emptiable
    import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
    import Scroll exposing (FocusGap, Scroll)
    import Stack

    type alias Model =
        RecordWithoutConstructorFunction
            { numbers : Scroll Int FocusGap Never
            }

    type Event
        = NumberClicked Scroll.Location

    update : Event -> Model -> ( Model, Cmd Event )
    update event model =
        case event of
            NumberClicked location ->
                ( { model
                    | numbers =
                        model.numbers
                            |> Scroll.to location
                            |> Emptiable.fillElseOnEmpty (\_ -> model.numbers)
                            |> Scroll.focusAlter (Emptiable.map (\n -> n + 1))
                  }
                , Cmd.none
                )

    interface : Model -> Ui.Element Event
    interface =
        \{ numbers } ->
            numbers
                |> Scroll.map numberInterface
                |> Scroll.toList
                |> Ui.column []

    numberInterface :
        Scroll.Location
        -> Int
        -> Ui.Element Event
    numberInterface location =
        \number ->
            UIn.button []
                { onPress = NumberClicked location |> Just
                , label =
                    number
                        |> String.fromInt
                        |> Ui.text
                }

The same _functionality_ is often provided as `duplicate : Scroll item ... -> Scroll (Scroll item) ...`

  - [turboMaCk/non-empty-list-alias: `List.NonEmpty.Zipper.duplicate`](https://package.elm-lang.org/packages/turboMaCk/non-empty-list-alias/latest/List-NonEmpty-Zipper#duplicate)
  - [miyamoen/select-list: `SelectList.selectedMap`](https://dark.elm.dmy.fr/packages/miyamoen/select-list/latest/SelectList#selectedMap)
  - [jjant/elm-comonad-zipper: `Zipper.duplicate`](https://package.elm-lang.org/packages/jjant/elm-comonad-zipper/latest/Zipper#duplicate)
  - [arowM/elm-reference: `Reference.List.unwrap`](https://dark.elm.dmy.fr/packages/arowM/elm-reference/latest/Reference-List#unwrap)

Saving a [`Scroll`](#Scroll) with every item becomes expensive for long [`Scroll`](#Scroll)s, though!

-}
to :
    Location
    ->
        (Scroll item FocusGap possiblyOrNever_
         -> Emptiable (Scroll item FocusGap never_) Possibly
        )
to location =
    \scroll ->
        case location of
            AtFocus ->
                scroll
                    |> focusItemTry
                    |> Emptiable.emptyAdapt (\_ -> Possible)

            AtSide side_ sideIndex ->
                case sideIndex of
                    0 ->
                        scroll |> toItemNearest side_

                    sideIndexNot0 ->
                        if sideIndexNot0 >= 1 then
                            scroll
                                |> toItemNearest side_
                                |> Emptiable.mapFlat
                                    (to (AtSide side_ (sideIndex - 1)))

                        else
                            Emptiable.empty


{-| Move the focus to the gap directly [`Down|Up`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/).
Feel free to [plug](#focusAlter) that gap right up!


#### `Scroll.toGap Down`

```monospace
🍍 <🍊> 🍉  ->  🍍 <> 🍊 🍉
```

    import Linear exposing (Direction(..))
    import Emptiable exposing (filled)
    import Stack exposing (topBelow)

    Scroll.one "world"
        |> Scroll.toGap Down
        |> Scroll.focusAlter (\_ -> "hello" |> filled)
        |> Scroll.toStack
    --> topBelow "hello" [ "world" ]


#### `Scroll.toGap Up`

```monospace
🍍 <🍊> 🍉  ->  🍍 🍊 <> 🍉
```

    import Linear exposing (Direction(..))
    import Emptiable exposing (filled)
    import Stack exposing (topBelow)

    Scroll.one "hello"
        |> Scroll.sideAlter Up
            (\_ -> Stack.one "world")
        |> Scroll.toGap Up
        |> Scroll.focusAlter (\_ -> filled "scrollable")
        |> Scroll.toStack
    --> topBelow "hello" [ "scrollable", "world" ]

-}
toGap :
    Linear.Direction
    ->
        (Scroll item FocusGap Never
         -> Scroll item FocusGap Possibly
        )
toGap side_ =
    \(BeforeFocusAfter before focus_ after) ->
        case side_ of
            Down ->
                BeforeFocusAfter
                    before
                    Emptiable.empty
                    (after |> onTopLay (focus_ |> fill))

            Up ->
                BeforeFocusAfter
                    (before |> onTopLay (focus_ |> fill))
                    Emptiable.empty
                    after


{-| Focus the furthest item [`Down/Up`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/) the focus

`Down`

```monospace
🍍 🍓 <🍊> 🍉  ->  <🍍> 🍓 🍊 🍉
```

`Up`

```monospace
🍓 <🍊> 🍉 🍇  ->  🍓 🍊 🍉 <🍇>
```

    import Linear exposing (Direction(..))
    import Emptiable exposing (Emptiable)
    import Stack exposing (Stacked, topBelow)

    Scroll.one 1
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ 3, 4 ])
        |> Scroll.sideAlter Down
            (\_ -> topBelow 4 [ 3, 2 ])
        |> Scroll.toEnd Down
        |> Scroll.focusItem
    --> 2

    Scroll.one 1
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ 3, 4 ])
        |> Scroll.toEnd Up
        |> Scroll.focusItem
    --> 4

    Scroll.one 1
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ 3, 4 ])
        |> Scroll.toEnd Up
        |> Scroll.side Down
    --> topBelow 3 [ 2, 1 ]
    --: Emptiable (Stacked number_) Possibly

-}
toEnd :
    Linear.Direction
    ->
        (Scroll item FocusGap possiblyOrNever
         -> Scroll item FocusGap possiblyOrNever
        )
toEnd end =
    \scroll ->
        let
            stackWithEndOnTop =
                case end of
                    Up ->
                        mirror

                    Down ->
                        identity
        in
        case scroll |> stackWithEndOnTop |> toStack |> Emptiable.map filled of
            Empty possiblyOrNever ->
                empty |> focusGapAdapt (\_ -> possiblyOrNever)

            Filled stackFilled ->
                case end of
                    Down ->
                        BeforeFocusAfter
                            Emptiable.empty
                            (stackFilled |> Stack.top |> filled)
                            (stackFilled |> Stack.removeTop)

                    Up ->
                        BeforeFocusAfter
                            (stackFilled |> Stack.removeTop)
                            (stackFilled |> Stack.top |> filled)
                            Emptiable.empty


{-| Focus the gap beyond the furthest item [`Down|Up`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/).
Remember that gaps surround everything!

`Down`

```monospace
🍍 🍓 <🍊> 🍉  ->  <> 🍍 🍓 🍊 🍉
```

`Up`

```monospace
🍓 <🍊> 🍉  ->  🍓 🍊 🍉 <>
```

    import Linear exposing (Direction(..))
    import Emptiable exposing (filled)
    import Stack exposing (topBelow)

    Scroll.one 1
            -- <1>
        |> Scroll.sideAlter Up
            (\_ -> topBelow 3 [ 4 ])
            -- <1> 3 4
        |> Scroll.toGap Up
            -- 1 <> 3 4
        |> Scroll.focusAlter (\_ -> filled 2)
            -- 1 <2> 3 4
        |> Scroll.toEndGap Down
            -- <> 1 2 3 4
        |> Scroll.focusAlter (\_ -> filled 0)
            -- <0> 1 2 3 4
        |> Scroll.toStack
    --> topBelow 0 [ 1, 2, 3, 4 ]

    Scroll.one 1
            -- <1>
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ 3 ])
            -- <1> 2 3
        |> Scroll.toEndGap Up
            -- 1 2 3 <>
        |> Scroll.focusAlter (\_ -> filled 4)
            -- 1 2 3 <4>
        |> Scroll.toStack
    --> topBelow 1 [ 2, 3, 4 ]

-}
toEndGap :
    Linear.Direction
    ->
        (Scroll item FocusGap possiblyOrNever_
         -> Scroll item FocusGap Possibly
        )
toEndGap side_ =
    \scroll ->
        case side_ of
            Down ->
                BeforeFocusAfter
                    Emptiable.empty
                    Emptiable.empty
                    (scroll
                        |> toStack
                        |> emptyAdapt (\_ -> Possible)
                    )

            Up ->
                BeforeFocusAfter
                    (scroll
                        |> mirror
                        |> toStack
                        |> emptyAdapt (\_ -> Possible)
                    )
                    Emptiable.empty
                    Emptiable.empty


{-| Move the focus to the nearest item [`Down|Up`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/) that matches a predicate.
If no such item was found return with [`Emptiable.empty`](Emptiable#empty)

    import Linear exposing (Direction(..))
    import Emptiable exposing (filled)
    import Stack exposing (topBelow)
    import Linear exposing (Direction(..))

    Scroll.one 4
        |> Scroll.sideAlter Down
            (\_ -> topBelow 2 [ -1, 0, 3 ])
        |> Scroll.toWhere ( Down, \_ item -> item < 0 )
        |> Emptiable.map Scroll.focusItem
    --> filled -1

    Scroll.one 4
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ -1, 0, 3 ])
        |> Scroll.toWhere ( Up, \_ item -> item < 0 )
        |> Emptiable.map focusItem
    --> filled -1

    Scroll.one -4
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ -1, 0, 3 ])
        |> Scroll.toWhere ( Up, \_ item -> item < 0 )
        |> Emptiable.map focusItem
    --> filled -4

-}
toWhere :
    ( Linear.Direction
    , { index : Int } -> item -> Bool
    )
    ->
        (Scroll item FocusGap possiblyOrNever_
         -> Emptiable (Scroll item FocusGap never_) Possibly
        )
toWhere ( side_, isFound ) =
    let
        scrollToNext next =
            \scroll ->
                scroll
                    |> toItemNearest side_
                    |> Emptiable.mapFlat
                        (toWhereFrom next)

        toWhereFrom { index } =
            \scroll ->
                case scroll |> focus of
                    Filled currentItem ->
                        if currentItem |> isFound { index = index } then
                            BeforeFocusAfter
                                (scroll |> side Down)
                                (filled currentItem)
                                (scroll |> side Up)
                                |> filled

                        else
                            scroll |> scrollToNext { index = index + 1 }

                    Empty _ ->
                        scroll |> scrollToNext { index = index + 1 }
    in
    toWhereFrom { index = 0 }


{-| Try to move the focus to the nearest item
[`Down|Up`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)

`Down`

```monospace
🍊 🍉 <🍓> 🍇  ->  🍊 <🍓> 🍉 🍇
🍊 🍉 <> 🍇  ->  🍊 <> 🍉 🍇
```

`Up`

```monospace
🍊 <🍓> 🍉 🍇  ->  🍊 🍉 <🍓> 🍇
🍊 <> 🍉 🍇  ->  🍊 🍉 <> 🍇
```

    import Linear exposing (Direction(..))
    import Emptiable exposing (Emptiable)
    import Scroll exposing (Scroll, FocusGap)

    Scroll.one 0
        |> Scroll.sideAlter Down
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.focusDrag Down
        |> Emptiable.mapFlat Scroll.toStack
    --> topBelow 3 [ 2, 0, 1 ]
    --: Emptiable (Stacked number_) Possibly

    Scroll.one 0
        |> Scroll.sideAlter Up
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.focusDrag Up
        |> Emptiable.mapFlat Scroll.toStack
    --> topBelow 1 [ 0, 2, 3 ]
    --: Emptiable (Stacked number_) Possibly

If there is no nearest item, the result is [`empty`](Emptiable#empty)

    import Linear exposing (Direction(..))
    import Emptiable
    import Stack exposing (topBelow)

    Scroll.one 0
        |> Scroll.sideAlter Up
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.focusDrag Down
    --> Emptiable.empty

    Scroll.one 0
        |> Scroll.sideAlter Down
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.focusDrag Up
    --> Emptiable.empty

-}
focusDrag :
    Linear.Direction
    ->
        (Scroll item FocusGap possiblyOrNever
         -> Emptiable (Scroll item FocusGap possiblyOrNever) Possibly
        )
focusDrag side_ =
    \scroll ->
        (scroll |> side side_)
            |> Emptiable.map filled
            |> Emptiable.map
                (\stackFilled ->
                    let
                        sideOppositeNew : Emptiable (Stacked item) Possibly
                        sideOppositeNew =
                            (scroll |> side (side_ |> Linear.opposite))
                                |> onTopLay (stackFilled |> top)

                        focus_ : Emptiable item possiblyOrNever
                        focus_ =
                            scroll |> focus

                        sideNew : Emptiable (Stacked item) Possibly
                        sideNew =
                            stackFilled |> removeTop
                    in
                    case side_ of
                        Down ->
                            BeforeFocusAfter sideNew focus_ sideOppositeNew

                        Up ->
                            BeforeFocusAfter sideOppositeNew focus_ sideNew
                )



--


{-| Look [`Down|Up`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)
the [`focus`](#focus) and operate directly an the [`Stack`](Stack) you see


#### `sideAlter Linear.Direction (\_ -> 🍒🍋)`

`Down`

```monospace
🍓 <🍊> 🍉
      ↓
🍋 🍒 <🍊> 🍉
```

`Up`

```monospace
🍍 🍓 <🍊> 🍉
      ↓
🍍 🍓 <🍊> 🍒 🍋
```

    import Linear exposing (Direction(..))
    import Emptiable
    import Stack exposing (topBelow)

    Scroll.one "selectoo"
        |> Scroll.sideAlter Down
            (\_ -> topBelow "earlee" [ "agua", "enutai" ])
        |> Scroll.sideAlter Up
            (\_ -> topBelow "orangloo" [ "iquipy", "oice" ])
        |> Scroll.sideAlter Up
            (\_ -> Emptiable.empty)
        |> Scroll.toStack
    --> topBelow "enutai" [ "agua", "earlee", "selectoo" ]


#### `sideAlter Linear.Direction (Stack.map ...)`

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow)

    Scroll.one "second"
        |> Scroll.sideAlter Down
            (\_ -> topBelow "first" [ "zeroth" ])
        |> Scroll.sideAlter Down
            (Stack.map (\_ -> String.toUpper))
        |> Scroll.toStack
    --> topBelow "ZEROTH" [ "FIRST", "second" ]

    Scroll.one "zeroth"
        |> Scroll.sideAlter Up
            (\_ -> topBelow "first" [ "second" ])
        |> Scroll.sideAlter Up
            (Stack.map (\_ -> String.toUpper))
        |> Scroll.toStack
    --> topBelow "zeroth" [ "FIRST", "SECOND" ]

Look to one [side](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/) from the focus
and slide items in directly at the nearest location


#### `sideAlter Linear.Direction (Stack.attach 🍒🍋)`

`Down`

```monospace
      🍒🍋
🍍 🍓 \↓/ <🍊> 🍉
```

`Up`

```monospace
        🍒🍋
🍓 <🍊> \↓/ 🍉 🍇
```

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow)

    Scroll.one 0
        |> Scroll.sideAlter Down
            (Stack.attach Down ([ -4, -5 ] |> Stack.fromList))
        |> Scroll.sideAlter Down
            (Stack.attach Down (topBelow -1 [ -2, -3 ]))
        |> Scroll.toStack
    --> topBelow -5 [ -4, -3, -2, -1, 0 ]

    Scroll.one 0
        |> Scroll.sideAlter Up
            (Stack.attach Down ([ 4, 5 ] |> Stack.fromList))
        |> Scroll.sideAlter Up
            (Stack.attach Down (topBelow 1 [ 2, 3 ]))
        |> Scroll.toStack
    --> topBelow 0 [ 1, 2, 3, 4, 5 ]


#### `Scroll.sideAlter Linear.Direction (Stack.attach Up 🍒🍋)`

`Down`

```monospace
🍋🍒
 \↓ 🍍 🍓 <🍊> 🍉
```

`Up`

```monospace
              🍒🍋
🍓 <🍊> 🍉 🍇 ↓/
```

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow)

    Scroll.one 1
        |> Scroll.sideAlter Up
            (Stack.attach Up (topBelow 2 [ 3, 4 ]))
        |> Scroll.toEnd Up
        |> Scroll.sideAlter Down
            (Stack.attach Up (topBelow 7 [ 6, 5 ]))
        |> Scroll.toStack
    --> topBelow 5 [ 6, 7, 1, 2, 3, 4 ]

    Scroll.one 123
        |> Scroll.sideAlter Up
            (Stack.attach Up (Stack.one 456))
        |> Scroll.sideAlter Up
            (Stack.attach Up (topBelow 789 [ 0 ]))
        |> Scroll.toStack
    --> topBelow 123 [ 456, 789, 0 ]


#### `sideAlter Linear.Direction (`[`Stack.onTopLay`](Stack#onTopLay) `...)`

`Down`

```monospace
      🍒
🍍 🍓 ↓ <🍊> 🍉
```


#### `Up`

```monospace
        🍒
🍓 <🍊> ↓ 🍉 🍇
```

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow, onTopLay)

    Scroll.one 123
        |> Scroll.sideAlter Down
            (onTopLay 456)
        |> Scroll.toStack
    --> topBelow 456 [ 123 ]

    Scroll.one 123
        |> Scroll.sideAlter Up
            (\_ -> Stack.one 789)
        |> Scroll.sideAlter Up
            (onTopLay 456)
        |> Scroll.toStack
    --> topBelow 123 [ 456, 789 ]

-}
sideAlter :
    Linear.Direction
    ->
        (Emptiable (Stacked item) Possibly
         -> Emptiable (Stacked item) possiblyOrNever_
        )
    ->
        (Scroll item FocusGap possiblyOrNever
         -> Scroll item FocusGap possiblyOrNever
        )
sideAlter sideFaced sideStackAlter =
    \scroll ->
        scroll
            |> focusSidesMap
                { side =
                    \stackSide ->
                        if stackSide /= sideFaced then
                            identity

                        else
                            \sideStack ->
                                sideStack
                                    |> sideStackAlter
                                    |> Emptiable.emptyAdapt (\_ -> Possible)
                , focus = identity
                }


{-| Swap the [stack](Stack) on the [`side Down`](#side) the [`focus`](#focus)
with the [stack](Stack) on the [`side Up`](#side)

```monospace
🍓 <🍊> 🍉 🍇  <->  🍇 🍉 <🍊> 🍓
```

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow)

    Scroll.one 1
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ 3, 4 ])
        |> Scroll.sideAlter Down
            (\_ -> topBelow 4 [ 3, 2 ])

In contrast to `List` or [stack](Stack), this can be done in `O(1)` time

-}
mirror :
    Scroll item FocusGap possiblyOrNever
    -> Scroll item FocusGap possiblyOrNever
mirror =
    \(BeforeFocusAfter before_ focus_ after_) ->
        BeforeFocusAfter after_ focus_ before_



-- transform


{-| Change every item based on its current value

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow)

    Scroll.one "first"
        |> Scroll.sideAlter Down
            (\_ -> Stack.one "zeroth")
        |> Scroll.sideAlter Up
            (\_ -> topBelow "second" [ "third" ])
        |> Scroll.map (\_ -> String.toUpper)
        |> Scroll.toStack
    --> topBelow "ZEROTH" [ "FIRST", "SECOND", "THIRD" ]

[`focusSidesMap`](#focusSidesMap) allows changing the individual parts separately

-}
map :
    (Location -> item -> mappedItem)
    ->
        (Scroll item FocusGap possiblyOrNever
         -> Scroll mappedItem FocusGap possiblyOrNever
        )
map changeItem =
    \scroll ->
        scroll
            |> focusSidesMap
                { side =
                    \side_ ->
                        Stack.map
                            (\{ index } ->
                                changeItem (AtSide side_ index)
                            )
                , focus = Emptiable.map (changeItem AtFocus)
                }


{-| Fold, starting from one end item as the initial accumulation value,
reducing in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow)

    Scroll.one 234
        |> Scroll.sideAlter Up
            (\_ -> topBelow 345 [ 543 ])
        |> Scroll.fold Up max
    --> 543

To accumulate into anything that doesn't have the same type as the [`Scroll`]s items,
[`foldFromOne`](#foldFromOne)

-}
fold :
    Linear.Direction
    -> (item -> (item -> item))
    ->
        (Scroll item FocusGap Never
         -> item
        )
fold direction reduce =
    \scroll ->
        scroll |> foldFromOne identity direction reduce


{-| Fold, starting from one end item transformed to the initial accumulation value,
reducing in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow)

    Scroll.one 234
        |> Scroll.sideAlter Up
            (\_ -> topBelow 345 [ 543 ])
        |> Scroll.foldFromOne Stack.one Down Stack.onTopLay
    --> topBelow 234 [ 345, 543 ]

A simpler version is

    Scroll.fold =
        Scroll.foldFromOne identity

-}
foldFromOne :
    (item -> accumulated)
    -> Linear.Direction
    -> (item -> (accumulated -> accumulated))
    ->
        (Scroll item FocusGap Never
         -> accumulated
        )
foldFromOne endInitialToAccumulator direction reduce =
    \scroll ->
        scroll
            |> side direction
            |> Stack.foldFrom
                (scroll
                    |> side (direction |> Linear.opposite)
                    |> onTopLay (scroll |> focusItem)
                    |> Stack.foldFromOne endInitialToAccumulator
                        direction
                        reduce
                )
                (direction |> Linear.opposite)
                reduce


{-| Reduce in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow)

    Scroll.one 'i'
        |> Scroll.sideAlter Down
            (\_ -> topBelow 'v' [ 'e' ])
        |> Scroll.sideAlter Up
            (\_ -> Stack.one 'l')
        |> Scroll.foldFrom "" Down String.cons
    --> "evil"

    Scroll.one 'v'
        |> Scroll.sideAlter Down
            (\_ -> Stack.one 'e')
        |> Scroll.sideAlter Up
            (\_ -> topBelow  'i' [ 'l' ])
        |> Scroll.foldFrom "" Up String.cons
    --> "live"

    Scroll.empty
        |> Scroll.sideAlter Down
            (\_ -> topBelow 'v' [ 'e' ])
        |> Scroll.sideAlter Up
            (\_ -> topBelow  'i' [ 'l' ])
        |> Scroll.foldFrom "" Up String.cons
    --> "live"

-}
foldFrom :
    accumulationValue
    -> Linear.Direction
    -> (item -> (accumulationValue -> accumulationValue))
    ->
        (Scroll item FocusGap possiblyOrNever_
         -> accumulationValue
        )
foldFrom accumulationValueInitial direction reduce =
    -- opposite side first in direction
    -- then side in opposite direction
    \scroll ->
        scroll
            |> side direction
            |> (case scroll |> focus of
                    Empty _ ->
                        identity

                    Filled focusItem_ ->
                        onTopLay focusItem_
               )
            |> Stack.foldFrom
                (scroll
                    |> side (direction |> Linear.opposite)
                    |> Stack.foldFrom accumulationValueInitial
                        Down
                        reduce
                )
                Up
                reduce


{-| Alter the focus – [item or gap](Emptiable) – based on its current value


#### `Scroll.focusAlter (\_ -> 🍊 |> filled)`

```monospace
🍊  ->  🍓 <?> 🍉  ->  🍓 <🍊> 🍉
```

    import Linear exposing (Direction(..))
    import Emptiable exposing (filled)
    import Stack exposing (topBelow, onTopLay)

    Scroll.empty
            -- <>
        |> Scroll.sideAlter Down
            (onTopLay "🍓")
            -- "🍓" <>
        |> Scroll.sideAlter Up
            (onTopLay "🍉")
            -- "🍓" <> "🍉"
        |> Scroll.focusAlter (\_ -> "🍊" |> filled)
            -- "🍓" <"🍊"> "🍉"
        |> Scroll.toStack
    --> topBelow "🍓" [ "🍊", "🍉" ]


#### `Scroll.focusAlter (\_ -> Emptiable.empty)`

```monospace
🍓 <?> 🍉  ->  🍓 <> 🍉
```

    import Linear exposing (Direction(..))
    import Emptiable exposing (filled)
    import Stack exposing (topBelow)

    Scroll.one "hello"
        |> Scroll.sideAlter Up
            (\_ -> topBelow "scrollable" [ "world" ])
        |> Scroll.to (Up |> Scroll.nearest)
        |> Emptiable.map (Scroll.focusAlter (\_ -> Emptiable.empty))
        |> Emptiable.map Scroll.toList
    --> filled [ "hello", "world" ]


#### `Scroll.focusAlter (?🍒 -> ?🍊)`

```monospace
(?🍒 -> ?🍊)  ->  🍓 <?🍒> 🍉  ->  🍓 <?🍊> 🍉
```

    import Linear exposing (Direction(..))
    import Emptiable exposing (filled)
    import Stack exposing (topBelow, onTopLay)

    Scroll.empty
            -- <>
        |> Scroll.sideAlter Down
            (onTopLay "🍓")
            -- "🍓" <>
        |> Scroll.sideAlter Up
            (onTopLay "🍉")
            -- "🍓" <> "🍉"
        |> Scroll.focusAlter
            (\_ -> filled "🍊")
            -- "🍓" <"🍊"> "🍉"
        |> Scroll.toStack
    --> topBelow "🍓" [ "🍊", "🍉" ]

    Scroll.one "first"
        |> Scroll.sideAlter Down
            (\_ -> Stack.one "zeroth")
        |> Scroll.sideAlter Up
            (\_ -> topBelow "second" [ "third" ])
        |> Scroll.focusAlter (Emptiable.map String.toUpper)
        |> Scroll.toStack
    --> topBelow "zeroth" [ "FIRST", "second", "third" ]

-}
focusAlter :
    (Emptiable item possiblyOrNever
     -> Emptiable item possiblyOrNeverAltered
    )
    ->
        (Scroll item FocusGap possiblyOrNever
         -> Scroll item FocusGap possiblyOrNeverAltered
        )
focusAlter focusHandAlter =
    \(BeforeFocusAfter before focus_ after) ->
        BeforeFocusAfter
            before
            (focus_ |> focusHandAlter)
            after


{-| Change the [`focus`](#focus),
the [`side`](#side)s `Down` and `Up`
using different functions

    import Linear exposing (Direction(..))
    import Emptiable exposing (filled)
    import Stack exposing (topBelow)

    Scroll.one "first"
        |> Scroll.sideAlter Up
            (\_ -> Stack.one "second")
        |> Scroll.toGap Up
        |> Scroll.focusAlter (\_ -> filled "one-and-a-halfth")
        |> Scroll.focusSidesMap
            { side =
                \side ->
                    Stack.map
                        (\_ item ->
                            String.concat
                                [ side |> sideToString, ": ", item ]
                        )
            , focus =
                map (\item -> "focused item: " ++ item)
            }
        |> Scroll.toStack
    --→
    topBelow
        "before: first"
        [ "focused item: one-and-a-halfth"
        , "after: second"
        ]

    sideToString =
        \side ->
            case side of
                Down ->
                    "before"

                Up ->
                    "after"

[`map`](#map) transforms every item

-}
focusSidesMap :
    { focus :
        Emptiable item possiblyOrNever
        -> Emptiable mappedItem possiblyOrNeverMapped
    , side :
        Linear.Direction
        ->
            (Emptiable (Stacked item) Possibly
             -> Emptiable (Stacked mappedItem) possiblyOrNeverMappedBefore_
            )
    }
    ->
        (Scroll item FocusGap possiblyOrNever
         -> Scroll mappedItem FocusGap possiblyOrNeverMapped
        )
focusSidesMap changeFocusAndSideStacks =
    \(BeforeFocusAfter sideBefore focus_ sideAfter) ->
        BeforeFocusAfter
            (sideBefore
                |> changeFocusAndSideStacks.side Down
                |> emptyAdapt (\_ -> Possible)
            )
            (focus_ |> changeFocusAndSideStacks.focus)
            (sideAfter
                |> changeFocusAndSideStacks.side Up
                |> emptyAdapt (\_ -> Possible)
            )


{-| Converts it to a `List`, rolled out to both ends:

    import Linear exposing (Direction(..))
    import Stack

    Scroll.one 456
        |> Scroll.sideAlter Down
            (\_ -> Stack.one 123)
        |> Scroll.sideAlter Up
            (\_ -> Stack.one 789)
        |> Scroll.toList
    --> [ 123, 456, 789 ]

Only use this if you need a list in the end.
Otherwise, use [`toStack`](#toStack) to preserve some information about its length

-}
toList : Scroll item FocusGap possiblyOrNever_ -> List item
toList =
    \scroll ->
        scroll
            |> toStack
            |> Stack.toList


{-| Roll out the `Scroll` to both ends into a [`Stack`](Stack):

    import Linear exposing (Direction(..))
    import Emptiable exposing (filled)
    import Stack exposing (topBelow)

    Scroll.empty
        |> Scroll.toStack
    --> Emptiable.empty

    Scroll.one 123
        |> Scroll.sideAlter Up
            (\_ -> Stack.one 789)
        |> Scroll.toGap Up
        |> Scroll.focusAlter (\_-> filled 456)
        |> Scroll.toStack
    --> topBelow 123 [ 456, 789 ]

its type information gets carried over, so

    Scroll.FocusGap Never -> Emptiable Never
    Scroll.FocusGap Possibly -> Emptiable Possibly

-}
toStack :
    Scroll item FocusGap possiblyOrNever
    -> Emptiable (Stacked item) possiblyOrNever
toStack =
    \scroll ->
        case scroll |> focus of
            Filled focusItem_ ->
                scroll
                    |> focusAlter (\_ -> focusItem_ |> filled)
                    |> foldFromOne Stack.one Down Stack.onTopLay

            Empty emptiable ->
                scroll
                    |> foldFrom (Empty emptiable) Down Stack.onTopLay



--


{-| [`Emptiable.empty`](Emptiable#empty) if the current focussed thing is a gap,
[`Emptiable.filled`](Emptiable#filled) if it's an item

    import Emptiable
    import Stack exposing (topBelow)
    import Linear exposing (Direction(..))

    Scroll.one 3
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ 1 ])
        |> Scroll.toGap Up
        |> Scroll.focusItemTry
    --> Emptiable.empty

-}
focusItemTry :
    Scroll item FocusGap possiblyOrNever
    -> Emptiable (Scroll item FocusGap never_) possiblyOrNever
focusItemTry =
    \scroll ->
        (scroll |> focus)
            |> Emptiable.map
                (\focusedItem_ ->
                    scroll |> focusAlter (\_ -> focusedItem_ |> filled)
                )



--


{-| Change the `possiblyOrNever` type

  - A `Scroll ... FocusGap possiblyOrNever`
    can't be used as a `Scroll ... Possibly`?

        import Possibly exposing (Possibly(..))

        Scroll.focusGapAdapt (always Possible)

  - A `Scroll ... FocusGap Never`
    can't be unified with `Scroll ... FocusGap Possibly` or an argument variable `FocusGap possiblyOrNever`?

        Scroll.focusGapAdapt never

Please read more at [`Emptiable.emptyAdapt`](Emptiable#emptyAdapt)

-}
focusGapAdapt :
    (possiblyOrNever -> possiblyOrNeverAdapted)
    ->
        (Scroll item FocusGap possiblyOrNever
         -> Scroll item FocusGap possiblyOrNeverAdapted
        )
focusGapAdapt neverOrAlwaysPossible =
    \scroll ->
        scroll
            |> focusAlter
                (Emptiable.emptyAdapt neverOrAlwaysPossible)
