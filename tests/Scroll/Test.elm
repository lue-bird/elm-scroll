module Scroll.Test exposing (tests)

import Emptiable exposing (Emptiable, filled)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Linear exposing (Direction(..))
import Scroll exposing (FocusGap, Scroll)
import Stack exposing (Stacked, removeTop, top)
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Scroll"
        [ describe "alter" [ attachToEndTest, mirrorTest ]
        , describe "navigate" [ focusItemTest ]
        , describe "transform" [ toStackTest ]
        ]



-- alter


mirrorTest : Test
mirrorTest =
    describe "mirror"
        [ Test.fuzz
            (Scroll.fuzz Fuzz.int)
            "(mirror >> mirror) = identity"
            (\scroll ->
                scroll
                    |> Scroll.mirror
                    |> Scroll.mirror
                    |> expectEqualScroll scroll
            )
        ]


attachToEndTest : Test
attachToEndTest =
    describe "sideAlter"
        [ describe "replace"
            [ Test.fuzz
                (Stack.filledFuzz Fuzz.int)
                "Down"
                (\stack ->
                    Scroll.one (stack |> top)
                        |> Scroll.sideAlter Down
                            (\_ -> stack |> removeTop)
                        |> Scroll.toStack
                        |> expectEqualStack (stack |> Stack.reverse)
                )
            , Test.fuzz
                (Stack.filledFuzz Fuzz.int)
                "Up"
                (\stack ->
                    Scroll.one (stack |> top)
                        |> Scroll.sideAlter Up
                            (\_ -> stack |> removeTop)
                        |> Scroll.toStack
                        |> expectEqualStack stack
                )
            ]
        ]



-- navigate focus


focusItemTest : Test
focusItemTest =
    let
        scrollFocusSideFuzz =
            Fuzz.constant
                (\focus_ sideStack side ->
                    Scroll.one focus_
                        |> Scroll.sideAlter side
                            (\_ -> sideStack)
                )
                |> Fuzz.andMap Fuzz.int
                |> Fuzz.andMap (Stack.filledFuzz Fuzz.int)

        sideAndScrollFuzz scrollFuzz_ =
            Fuzz.constant
                (\side scroll ->
                    { side = side
                    , scroll = scroll side
                    }
                )
                |> Fuzz.andMap sideFuzz
                |> Fuzz.andMap scrollFuzz_
    in
    describe "Scroll.to"
        [ Test.fuzz
            (sideAndScrollFuzz scrollFocusSideFuzz)
            "items exist on the Linear.Direction"
            (\{ scroll, side } ->
                scroll
                    |> Scroll.to (side |> Scroll.nearest)
                    |> Emptiable.mapFlat (Scroll.side side)
                    |> Expect.equal
                        (scroll
                            |> Scroll.side side
                            |> Emptiable.mapFlat (Stack.removeTop << filled)
                        )
            )
        , Test.fuzz
            (sideAndScrollFuzz scrollFocusSideFuzz)
            "no items on the Linear.Direction"
            (\{ scroll, side } ->
                scroll
                    |> Scroll.to
                        (side |> Linear.directionOpposite |> Scroll.nearest)
                    |> Expect.equal Emptiable.empty
            )
        , Test.fuzz
            (sideAndScrollFuzz scrollFocusSideFuzz)
            "`(Scroll.toEndGap Linear.Direction >> Scroll.to (Linear.Direction |> Linear.directionOpposite)) == Scroll.toEnd Linear.Direction`"
            (\{ scroll, side } ->
                scroll
                    |> Scroll.toEndGap side
                    |> Scroll.to
                        (side |> Linear.directionOpposite |> Scroll.nearest)
                    |> Expect.equal
                        (scroll |> Scroll.toEnd side |> filled)
            )
        , Test.fuzz
            (sideAndScrollFuzz scrollFocusSideFuzz)
            "`(Scroll.toGap Linear.Direction >> Scroll.to Linear.Direction) == Scroll.to Linear.Direction`"
            (\{ scroll, side } ->
                scroll
                    |> Scroll.toGap side
                    |> Scroll.to (side |> Scroll.nearest)
                    |> Expect.equal
                        (scroll |> Scroll.to (side |> Scroll.nearest))
            )
        , Test.fuzz
            (sideAndScrollFuzz scrollFocusSideFuzz)
            "`(Scroll.toEnd Linear.Direction >> Scroll.to Linear.Direction) == Emptiable.empty`"
            (\{ scroll, side } ->
                scroll
                    |> Scroll.toEnd side
                    |> Scroll.to (side |> Scroll.nearest)
                    |> Expect.equal Emptiable.empty
            )
        , Test.fuzz
            (sideAndScrollFuzz scrollFocusSideFuzz)
            "repeating `Scroll.to (Linear.Direction |> Scroll.nearest)` length times results in empty"
            (\{ scroll, side } ->
                Scroll.to (side |> Scroll.nearest)
                    |> List.repeat
                        (1 + (scroll |> Scroll.side side |> Stack.length))
                    |> List.foldl Emptiable.mapFlat (scroll |> filled)
                    |> Expect.equal Emptiable.empty
            )
        ]



-- transform


toStackTest : Test
toStackTest =
    describe "toStack"
        [ test "empty"
            (\_ ->
                Scroll.empty
                    |> Scroll.toStack
                    |> Expect.equal Emptiable.empty
            )
        , test "only"
            (\_ ->
                Scroll.one 3
                    |> Scroll.toStack
                    |> Expect.equal (Stack.one 3)
            )
        ]



-- common


expectEqualStack :
    Emptiable (Stacked element) possiblyOrNever
    -> Emptiable (Stacked element) possiblyOrNever
    -> Expectation
expectEqualStack expectedStack =
    \actualStack ->
        actualStack
            |> Expect.equal expectedStack


expectEqualScroll :
    Scroll item FocusGap possiblyOrNever
    -> Scroll item FocusGap possiblyOrNever
    -> Expectation
expectEqualScroll expectedScroll =
    \actualScroll ->
        actualScroll
            |> Expect.equal expectedScroll


sideFuzz : Fuzzer Linear.Direction
sideFuzz =
    Fuzz.oneOf
        [ Down |> Fuzz.constant
        , Up |> Fuzz.constant
        ]
