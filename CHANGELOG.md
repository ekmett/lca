0.4 [yyyy.mm.dd]
-----------------
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.
* Exported `null` and `length` are always specific to `Path`, i.e
  
  ```haskell
  length :: Path a -> Int
  null :: Path a -> Bool
  ```

  Previously, these specialized versions where exported with GHC-7.8 and older;
  for more recent GHCs we exported more generic `Data.Foldable` variants.
  Now the exported API is uniform across supported GHC version range.

0.3.1 [2018.02.06]
------------------
* Fix the build with GHC 8.4.
* Use `cabal-doctest` for the test suite.

0.3
---
* Updated to build without warnings in the wake of GHC 7.10.
* Use (and re-export) the new overloaded `null` and `length` from Prelude on GHC 7.10+
* Modified `mkeep`, `mdrop` and `mlca` to parameterize them by monoid homomorphisms. This permits cheaper summaries to be calculated over the dropped path, when only a portion of the information in the path is required.

0.2.4
-----
* Fixed a bug in path reconstruction

0.2.3
-----
* Improved documentation to also note that this package also provides an improvement in the online version of the [level ancestor problem](http://en.wikipedia.org/wiki/Level_ancestor_problem).

0.2.2
-----
* Added README
* Better haddock coverage
* Added links to documentation
