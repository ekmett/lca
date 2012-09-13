lca
===

[![Build Status](https://secure.travis-ci.org/ekmett/lca.png?branch=master)](http://travis-ci.org/ekmett/lca)

This package provides a reference implementation of my skew binary random access algorithm for performing an
online lowest common ancestor in logarithmic time without preprocessing. This improves the previous known
asymptotic bound from /O(h)/ to /O(log h)/, and importantly is completely independent of the width or overall
size of the tree, enabling you to calculate lowest common ancestors in a distributed fashion with good locality.

<iframe src="http://www.slideshare.net/slideshow/embed_code/13144385" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC;border-width:1px 1px 0;margin-bottom:5px" allowfullscreen>Test</iframe> <div style="margin-bottom:5px"> <strong> <a href="http://www.slideshare.net/ekmett/skewbinary-online-lowest-common-ancestor-search" title="Skew-binary On-line Lowest Common Ancestor Search" target="_blank">Skew-binary On-line Lowest Common Ancestor Search</a> </strong> from <strong><a href="http://www.slideshare.net/ekmett" target="_blank">Edward Kmett</a></strong> </div>

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
