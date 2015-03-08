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
