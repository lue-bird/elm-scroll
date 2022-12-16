# change log

# 1.0.0
moved `Scroll` from `emptiness-typed`

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
