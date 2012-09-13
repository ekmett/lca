lca: Purely Functional On-Line Lowest Common Ancestor Search
============================================================

[![Build Status](https://secure.travis-ci.org/ekmett/lca.png?branch=master)](http://travis-ci.org/ekmett/lca)

This package provides a reference implementation of my skew binary random access algorithm for performing an
online lowest common ancestor in logarithmic time without preprocessing. This improves the previous known
asymptotic bound from _O(h)_ to _O(log h)_, and importantly is completely independent of the width or overall
size of the tree, enabling you to calculate lowest common ancestors in a distributed fashion with good locality.

Slides are available as [Purely Functional Data Structures for On-Line LCA](http://www.slideshare.net/ekmett/skewbinary-online-lowest-common-ancestor-search)

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
