## Version 0.9.1.1 (2026-11-23)

- Bump dependency upper bounds, more tests and CI improvements.
- Make Radix instances total (mainly to appease the compiler).
- AmericanFlag: Fix Lexicographic instance for pairs.
- AmericanFlag: Fix pair ordering for variable-length first components.

## Version 0.9.1.0 (2025-02-05)

- More inlining for `sort` and `nib` functions.

## Version 0.9.0.3 (2024-11-25)

- Fix an off-by-one error Heap.partialSort functions.
- Support latest ghcs.

## Version 0.9.0.2 (2024-05-23)

- Add `TypeOperators` pragma where needed.

## Version 0.9.0.1 (2022-07-28)

- Allow building with vector-0.13.*.

## Version 0.9.0.0 (2022-05-19)

- Add nub related functions.
- Add sortUniq related functions (sorts, then removes duplicates).

## Version 0.8.0.4 (2020-12-06)

- Fix out of range access in Intro.partialSort.
- Update QuickCheck dependency bounds.

## Version 0.8.0.3 (2019-12-02)

- Fix out-of-bounds access in Timsort.

## Version 0.8.0.2 (2019-11-28)

- Bump upper bounds on primitive and QuickCheck.
- Expose 'terminate' function from 'AmericanFlag' module.
- Fix an off-by-one error in Data.Vector.Algorithms.Heaps.heapInsert.

