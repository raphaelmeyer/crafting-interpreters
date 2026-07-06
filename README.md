# Crafting Interpreters

I joined a book club of the [Coders Only](https://codersonly.org/) association
reading and discussing
[Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom.
We met every other week (or at even longer intervals due to holidays) to
discuss one chapter at the time.

I wrote my own implementations while reading a chapter. Because of the stretched
timeline I tried to stick closely to the book's implementation in order to find
the spot where changes where required later in the book. Unfortunately, this
resulted in quirky and awkward implementations, particularly the C++ and Ada
versions.

## Tree-Walk Interpreter

In assumed to learn more, if I use another language instead of the copying the
Java code. I choose Haskell, just because. That forced me to translate some of
the concept and deviate further from the book. Still, because I tried to stay
close the the book's implementation, the result is not Haskell as I would
normally write.

## Bytecode Virtual Machine

I decided to challenge myself with three implementations C, C++ and Ada.

### C

The C version is almost the same as the book's implementation, except for a
different naming conventions, no macros and few small changes better matching my
taste.

### C++

The C++ version started as a copy of the C version, but with using data
structures from the standard library (`std::vector`, `std::array` etc.) and
replacing raw pointers with iterators. That worked well for the first few
chapters, but completely broke after the objects turned more complex (functions,
closures, upvalues you name it).

The result is a very brittle and hard to understand mess.

Still, the C++ implementation took one interesting turn. I used
`std::shared_ptr` instead of the raw pointers to objects. Now, as there are
cycles, the reference counted shared pointers leak memory. It was one of the
highlights to replace the usages to weak pointers and implement and adapted
version of the garbage collector to clean up the shared pointers stuck in
cycles.

### Ada

I started the Ada implementation with the naive goal of using Ada idioms while
staying close to the book's implementation. In favor of staying close to the
original, I also had to give up on idiomatic Ada. While, this implementation
still looks better than the C++ version, I wouldn't consider it good Ada code.
It is just a one-to-one translation of C, shamelessly using to many `Access`
types.

## Summary

If you're reading this in preparation for working your own way through the book,
here's some of my personal advice:

### Test Suite

There is a [test suite](https://github.com/munificent/craftinginterpreters) from
the author himself. Look for `tool/bin/test.dart`. It may take some time the set
up a legacy version of dart, or migrate the script to a newer version of dart. I
discovered the test suite when I was already in the middle of the tree-walking
interpreter. I wished I had found it earlier, it really helped a lot.

### Tree-Walk Interpreter

In my opinion you learn more if you're not just copying the code from the book.
My advice is to either use a language that translates well from Java, e.g. C#,
go, C++ etc. and code along while reading. Or, if you are more adventurous,
first read the whole part and only start implementing it in a language of
different paradigms afterwards. Or maybe you want to do both.

### Bytecode Virtual Machine

This part of the book has a few paragraphs that are very specific to C. My
recommendation is to first code along in C while reading. Then you may still try
to implement the compiler and VM in your language of choice afterwards.
