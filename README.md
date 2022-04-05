Holfenstein 3D: Wolfenstein 3D in Haskell + C
======

<p align="center">
  <img src="https://raw.githubusercontent.com/GregoryTravis/holfenstein/master/screenshots/holfenstein.gif">
</p>

This is one of my first Haskell projects; I first wrote all of the renderer in Haskell, then translated the hot spots to C.

Even the Haskell is pretty low-level in certain areas, just raw pointer reads and writes, so even in Haskell I got some of those classic renderer errors like:

* segmentation fault
* rendering is striped and way too tall
* etc.

I use the following high-quality dataset for the world model:

```
ssssssssssssssssssssssssssssssssssssssssssssssssss
sssssssggggggggggggggggggggggggggggggsssss sssssss
sssssssg                     ggggggggsssss sssssss
sssssssg ggggggggggggggggggggggggggggsssss sssssss
sssssssg gssssssssssssssssssssssssssssssss sssssss  <-- map.txt
sssssssg gssssssbbbfbbsssssssssbttbssss      gssss
sssssssg gmmmmmm      sssssssss    ssss   m   gsss
sssssssg                                      gsss
sssssssgssmmmmmm sssss   ssssss    ssss      gssss
ssssssssssssssssbsssssbbbssssssbttbsssssssssssssss
```

It's really true: ["In short, Haskell is the world’s finest imperative programming language." -- Simon Peyton Jones](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf?from=https%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fmarktoberdorf%2Fmark.pdf)

<p align="center">
  <img src="https://raw.githubusercontent.com/GregoryTravis/holfenstein/master/screenshots/wolf0.png">
</p>
<p align="center">
  <img src="https://raw.githubusercontent.com/GregoryTravis/holfenstein/master/screenshots/wolf1.png">
</p>
