# purescript-resize-arrays

![logo](docs/logo.svg)


The `resize-arrays` library provides an array-like data structure that supports efficient resizing from both ends. Unlike standard arrays, which can be slow when inserting or removing elements from the front, this library ensures that adding and removing items at both the front and back operate in constant time (O(1)). 

Folding and mapping operations in this library tend to be slower than those in List or Array. However, their time complexity remains O(n).

[CPU Benchmarks](https://m-bock.github.io/purescript-resize-arrays/benchmarks.html)
