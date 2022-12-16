module Scroll exposing
    ( Scroll(..), FocusGap(..)
    , Location(..), nearest
    , empty, one
    , fuzz, focusFilledFuzz
    , focusFill, focus
    , side
    , length
    , to, toGap
    , toEnd, toEndGap
    , toWhere
    , dragFocus
    , mirror
    , focusAlter, sideAlter
    , map, focusSidesMap
    , focusFilled
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
@docs fuzz, focusFilledFuzz


## scan

@docs focusFill, focus
@docs side
@docs length


## move the focus

@docs to, toGap
@docs toEnd, toEndGap
@docs toWhere
@docs dragFocus


### alter

@docs mirror
@docs focusAlter, sideAlter
@docs map, focusSidesMap
@docs focusFilled


## transform

@docs foldFrom, fold, foldFromOne
@docs toStack, toList


## type-level

@docs focusGapAdapt


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

-}

import Emptiable exposing (Emptiable(..), emptyAdapt, fill, filled)
import Fuzz exposing (Fuzzer)
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

(where [`RecordWithoutConstructorFunction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/)
stops the compiler from creating a constructor function for `Model`)

-}
type Scroll item focusGapTag possiblyOrNever
    = Scroll
        { before : Emptiable (Stacked item) Possibly
        , focus : Emptiable item possiblyOrNever
        , after : Emptiable (Stacked item) Possibly
        }


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
        |> Emptiable.map Scroll.focusFill
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
        |> Emptiable.map Scroll.focusFill
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
    Scroll
        { before = Emptiable.empty
        , focus = Emptiable.empty
        , after = Emptiable.empty
        }


{-| A `Scroll` with a single focussed item in it,
nothing `Down` and `Up` it

```monospace
🍊  ->  <🍊>
```

    import Stack

    Scroll.one "wat" |> Scroll.focusFill
    --> "wat"

    Scroll.one "wat" |> Scroll.toStack
    --> Stack.one "wat"

-}
one : element -> Scroll element FocusGap never_
one currentItem =
    Scroll
        { before = Emptiable.empty
        , after = Emptiable.empty
        , focus = filled currentItem
        }


{-| `Scroll item FocusGap Possibly`
[`Fuzzer`](https://dark.elm.dmy.fr/packages/elm-explorations/test/latest/Fuzz#Fuzzer)
with a given `item` `Fuzzer`.

Each side's generated length will be <= 32, The focus is either filled or not

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow)
    import Fuzz

    Scroll.fuzz (Fuzz.intRange 0 9) |> Fuzz.examples 2
    --> [ Scroll.one 6
    -->     |> Scroll.sideAlter Down (\_ -> topBelow 0 [ 1, 6, 0, 5, 1, 7, 8, 8, 0, 5, 4, 9, 4, 9, 3, 7, 0, 3, 6, 4 ])
    -->     |> Scroll.sideAlter Up (\_ -> topBelow 5 [ 9 ])
    --> , Scroll.empty
    -->     |> Scroll.sideAlter Down (\_ -> topBelow 3 [ 9, 7, 5, 0, 3 ])
    -->     |> Scroll.sideAlter Up (\_ -> topBelow 8 [ 4, 5, 6, 9, 6, 0, 6, 4, 3, 8, 0, 5, 4 ])
    --> ]

To always fuzz the focus as filled → [`focusFilledFuzz`](#focusFilledFuzz)

-}
fuzz : Fuzzer item -> Fuzzer (Scroll item FocusGap Possibly)
fuzz itemFuzz =
    Fuzz.constant
        (\before focus_ after ->
            Scroll
                { before = before
                , focus = focus_
                , after = after
                }
        )
        |> Fuzz.andMap (Stack.fuzz itemFuzz)
        |> Fuzz.andMap (Emptiable.fuzz itemFuzz)
        |> Fuzz.andMap (Stack.fuzz itemFuzz)


{-| `Scroll item FocusGap never_`
[`Fuzzer`](https://dark.elm.dmy.fr/packages/elm-explorations/test/latest/Fuzz#Fuzzer)
with a given `item` `Fuzzer`.

Each side's generated length will be <= 32, The focus is always filled

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow)
    import Fuzz

    Scroll.focusFilledFuzz (Fuzz.intRange 0 9)
        |> Fuzz.examples 2
    --> [ Scroll.one 5
    -->     |> Scroll.sideAlter Down (\_ -> topBelow 6 [ 7, 7, 5, 4, 1, 3, 9, 4, 0, 8, 8, 8, 5, 7, 9, 9 ])
    -->     |> Scroll.sideAlter Up (\_ -> topBelow 5 [ 9 ])
    --> , Scroll.one 5
    -->     |> Scroll.sideAlter Down (\_ -> topBelow 9 [ 9, 6, 4, 4, 9, 8, 0, 5, 1, 2, 2, 5, 9, 9, 3, 0 ])
    -->     |> Scroll.sideAlter Up (\_ -> topBelow 2 [ 7, 8 ])
    --> ]

To only sometimes fuzz the focus as filled → [`fuzz`](#fuzz)

-}
focusFilledFuzz : Fuzzer item -> Fuzzer (Scroll item FocusGap never_)
focusFilledFuzz itemFuzz =
    Fuzz.constant
        (\before focusItem_ after ->
            Scroll
                { before = before
                , focus = focusItem_ |> filled
                , after = after
                }
        )
        |> Fuzz.andMap (Stack.fuzz itemFuzz)
        |> Fuzz.andMap itemFuzz
        |> Fuzz.andMap (Stack.fuzz itemFuzz)



--


{-| The focused item

```monospace
🍍 🍓 <🍊> 🍉 🍇  ->  🍊
```

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow)

    Scroll.one "hi there" |> Scroll.focusFill
    --> "hi there"

    Scroll.one 1
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ 3, 4 ])
        |> Scroll.toEnd Up
        |> Scroll.focusFill
    --> 4

Short for

    import Emptiable

    Scroll.focusFill =
        Scroll.focus >> Emptiable.fill

-}
focusFill : Scroll item FocusGap Never -> item
focusFill =
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

[`focusFill`](#focusFill) is short for `focus |> fill`

-}
focus :
    Scroll item FocusGap possiblyOrNever
    -> Emptiable item possiblyOrNever
focus =
    \(Scroll scroll) -> scroll.focus


{-| The [stack](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)
to one [side](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)
of the focus

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
    \(Scroll scroll) ->
        case sideToAccess of
            Down ->
                scroll.before

            Up ->
                scroll.after


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
    \(Scroll scroll) ->
        (scroll.before |> Stack.length)
            + (case scroll.focus of
                Filled _ ->
                    1

                Empty _ ->
                    0
              )
            + (scroll.after |> Stack.length)



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
                            Scroll { before = sideNew, focus = focusNew, after = sideOppositeNew }

                        Up ->
                            Scroll { before = sideOppositeNew, focus = focusNew, after = sideNew }
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
        |> Emptiable.map Scroll.focusFill
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
        |> Emptiable.map Scroll.focusFill
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

If there is no next item,
the result is [`empty`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#empty)

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
                    |> focusFilled
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
    \(Scroll scroll) ->
        case side_ of
            Down ->
                Scroll
                    { before = scroll.before
                    , focus = Emptiable.empty
                    , after =
                        scroll.after |> onTopLay (scroll.focus |> fill)
                    }

            Up ->
                Scroll
                    { before =
                        scroll.before |> onTopLay (scroll.focus |> fill)
                    , focus = Emptiable.empty
                    , after = scroll.after
                    }


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
        |> Scroll.focusFill
    --> 2

    Scroll.one 1
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ 3, 4 ])
        |> Scroll.toEnd Up
        |> Scroll.focusFill
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
                        Scroll
                            { before = Emptiable.empty
                            , focus = stackFilled |> Stack.top |> filled
                            , after = stackFilled |> Stack.removeTop
                            }

                    Up ->
                        Scroll
                            { before = stackFilled |> Stack.removeTop
                            , focus = stackFilled |> Stack.top |> filled
                            , after = Emptiable.empty
                            }


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
                Scroll
                    { before = Emptiable.empty
                    , focus = Emptiable.empty
                    , after =
                        scroll
                            |> toStack
                            |> emptyAdapt (\_ -> Possible)
                    }

            Up ->
                Scroll
                    { before =
                        scroll
                            |> mirror
                            |> toStack
                            |> emptyAdapt (\_ -> Possible)
                    , focus = Emptiable.empty
                    , after = Emptiable.empty
                    }


{-| Move the focus to the nearest item [`Down|Up`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)
that matches a predicate.
If no such item was found,
[`Emptiable.empty`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#empty)

    import Linear exposing (Direction(..))
    import Emptiable exposing (filled)
    import Stack exposing (topBelow)
    import Linear exposing (Direction(..))

    Scroll.one 4
        |> Scroll.sideAlter Down
            (\_ -> topBelow 2 [ -1, 0, 3 ])
        |> Scroll.toWhere ( Down, \_ item -> item < 0 )
        |> Emptiable.map Scroll.focusFill
    --> filled -1

    Scroll.one 4
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ -1, 0, 3 ])
        |> Scroll.toWhere ( Up, \_ item -> item < 0 )
        |> Emptiable.map focusFill
    --> filled -1

    Scroll.one -4
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ -1, 0, 3 ])
        |> Scroll.toWhere ( Up, \_ item -> item < 0 )
        |> Emptiable.map focusFill
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
                            Scroll
                                { before = scroll |> side Down
                                , focus = filled currentItem
                                , after = scroll |> side Up
                                }
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
        |> Scroll.dragFocus Down
        |> Emptiable.mapFlat Scroll.toStack
    --> topBelow 3 [ 2, 0, 1 ]
    --: Emptiable (Stacked number_) Possibly

    Scroll.one 0
        |> Scroll.sideAlter Up
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.dragFocus Up
        |> Emptiable.mapFlat Scroll.toStack
    --> topBelow 1 [ 0, 2, 3 ]
    --: Emptiable (Stacked number_) Possibly

If there is no nearest item, the result is
[`empty`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#empty)

    import Linear exposing (Direction(..))
    import Emptiable
    import Stack exposing (topBelow)

    Scroll.one 0
        |> Scroll.sideAlter Up
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.dragFocus Down
    --> Emptiable.empty

    Scroll.one 0
        |> Scroll.sideAlter Down
            (\_ -> topBelow 1 [ 2, 3 ])
        |> Scroll.dragFocus Up
    --> Emptiable.empty

-}
dragFocus :
    Linear.Direction
    ->
        (Scroll item FocusGap possiblyOrNever
         -> Emptiable (Scroll item FocusGap possiblyOrNever) Possibly
        )
dragFocus side_ =
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

                        sideNew : Emptiable (Stacked item) Possibly
                        sideNew =
                            stackFilled |> removeTop
                    in
                    case side_ of
                        Down ->
                            Scroll
                                { before = sideNew
                                , focus = scroll |> focus
                                , after = sideOppositeNew
                                }

                        Up ->
                            Scroll
                                { before = sideOppositeNew
                                , focus = scroll |> focus
                                , after = sideNew
                                }
                )



--


{-| Look [`Down|Up`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)
the [`focus`](#focus) and operate directly
an the [stack](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)
you see


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


#### `sideAlter Linear.Direction (`[`Stack.onTopLay`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack#onTopLay) `...)`

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


{-| Swap the [stack](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)
on the [`side Down`](#side) the [`focus`](#focus)
with the [stack](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)
on the [`side Up`](#side)

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
        |> Scroll.mirror
    --> Scroll.one 1
    -->     |> Scroll.sideAlter Down
    -->         (\_ -> topBelow 2 [ 3, 4 ])
    -->     |> Scroll.sideAlter Up
    -->         (\_ -> topBelow 4 [ 3, 2 ])

In contrast to `List` or [stack](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack),
runtime is `O(1)`

-}
mirror :
    Scroll item FocusGap possiblyOrNever
    -> Scroll item FocusGap possiblyOrNever
mirror =
    \(Scroll scroll) ->
        Scroll
            { focus = scroll.focus
            , before = scroll.after
            , after = scroll.before
            }



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
                    |> onTopLay (scroll |> focusFill)
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


{-| Alter the focus
– [item or gap](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable) –
based on its current value


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
     -> Emptiable item alteredPossiblyOrNever
    )
    ->
        (Scroll item FocusGap possiblyOrNever
         -> Scroll item FocusGap alteredPossiblyOrNever
        )
focusAlter focusHandAlter =
    \(Scroll scroll) ->
        Scroll
            { before = scroll.before
            , focus = scroll.focus |> focusHandAlter
            , after = scroll.after
            }


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
    \(Scroll scroll) ->
        Scroll
            { before =
                scroll.before
                    |> changeFocusAndSideStacks.side Down
                    |> emptyAdapt (\_ -> Possible)
            , focus = scroll.focus |> changeFocusAndSideStacks.focus
            , after =
                scroll.after
                    |> changeFocusAndSideStacks.side Up
                    |> emptyAdapt (\_ -> Possible)
            }


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


{-| Roll out the [`Scroll`](#Scroll) to both ends
into a [`Stack`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)

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


{-| If the current focussed thing is a gap,
[`Emptiable.empty`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#empty).
If it's an item,
[`Emptiable.filled`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#filled)

    import Emptiable
    import Stack exposing (topBelow)
    import Linear exposing (Direction(..))

    Scroll.one 3
        |> Scroll.sideAlter Up
            (\_ -> topBelow 2 [ 1 ])
        |> Scroll.toGap Up
        |> Scroll.focusFilled
    --> Emptiable.empty

-}
focusFilled :
    Scroll item FocusGap possiblyOrNever
    -> Emptiable (Scroll item FocusGap never_) possiblyOrNever
focusFilled =
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

Please read more
at [`Emptiable.emptyAdapt`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#emptyAdapt)

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
