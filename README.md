# purescript-resize-arrays

The `resize-arrays` library provides an array-like data structure that supports efficient resizing from both ends. Unlike standard arrays, which can be slow when inserting or removing elements from the front, this library ensures that adding and removing items at both the front and back operate in constant time (O(1)). 

[CPU Benchmarks](https://html-preview.github.io/?url=https://github.com/m-bock/purescript-resize-arrays/blob/main/docs/benchmarks.html)