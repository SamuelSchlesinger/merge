# Revision history for merge

## 0.4.0.0

* Generalize `Merge f x a` to be parameterized by an arbitrary effect `f`.
  Use `Validation` for error-accumulating, `Either` for fail-fast, `Maybe`,
  `[]`, or any other `Applicative`.
* Add `hoist` for changing the effect via a natural transformation.
* Add `flatten` and `flattenValidation` for collapsing nested effects.
* Add generic deriving via `genericMerge` and the `GMerge` class.
* Add QuickCheck property-based test suite.

## 0.1.0.0

* First version. Released on an unsuspecting world.
