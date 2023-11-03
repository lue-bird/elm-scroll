# change log

#### 1.0.5

  - readme "alternatives" section: update to STTR13/ziplist to staeter/ziplist

#### 1.0.4

  - readme "alternatives" section mentions `stoeffel/list-focus`

#### 1.0.3

  - `linear-direction` → >= 11.0.0

#### 1.0.2

  - examples link update

#### 1.0.1

  - section alternatives move to readme

## 1.0.0
move `Scroll` from `emptiness-typed`

  - `toWhere ( side, isFound )` → `toWhen side isFound`
      - side is in no way directly related to the find function
  - `only` name → `one`
  - `foldOnto` name → `foldFromOne`
  - `focusItem` name → `focusFill`
      - consistent with `focus >> fill`
  - `focusItemTry` name → `focusFilled`
      - less confusing
  - `focusDrag` name → `dragFocus`
      - structure change, not change on focus
  - variant `FocusGap Never` expose
  - `fuzz`, `focusFilledFuzz` add
