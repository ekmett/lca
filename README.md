lca: _O(log h)_ Online Lowest Common Ancestor Search
====================================================

[![Hackage](https://img.shields.io/hackage/v/lca.svg)](https://hackage.haskell.org/package/lca) [![Build Status](https://github.com/ekmett/lca/workflows/Haskell-CI/badge.svg)](https://github.com/ekmett/lca/actions?query=workflow%3AHaskell-CI)

This package provides a reference implementation of my skew binary random access algorithm for performing an
online lowest common ancestor in logarithmic time _without preprocessing_. This improves the previous known
asymptotic bound for this problem from _O(h)_ to _O(log h)_, where _h_ is the height of the tree. Mostly
importantly this bound is completely independent of the width or overall size of the tree, enabling you to
calculate lowest common ancestors in a distributed fashion with good locality.

While algorithms exist that that provide _O(1)_ query time, they all require _O(n)_ preprocessing, where _n_ is
the size of the entire tree, and so are less suitable for LCA search in areas such as revision control where the
tree is constantly updated, or distributed computing where the tree may be too large to fit in any one computer's
memory.

Slides are available as [Purely Functional Data Structures for On-Line LCA](http://www.slideshare.net/ekmett/skewbinary-online-lowest-common-ancestor-search)

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
