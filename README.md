Holfenstein 3D: Wolfenstein 3D in Haskell + C
======

This is one of my first Haskell projects; I first wrote all of the renderer in Haskell, then translated the hot spots to C.

Even the Haskell is pretty low-level in certain areas, just raw pointer reads and writes, so even in Haskell I got some of those classic renderer errors like:

* segmentation fault
* rendering is striped and way to tall
* etc.

It's really true: ["In short, Haskell is the world’s finest imperative programming language." -- Simon Peyton Jones](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf?from=https%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fmarktoberdorf%2Fmark.pdf)
