# alox

## Resources

- [Introduction to Ada](https://learn.adacore.com/courses/intro-to-ada/index.html)
- [Alire](https://alire.ada.dev/) package managers

## Challenges

### Chapter 23 Challenge 1

Added new op code `OP_PUSH` that duplicates the value on the stack.

Statement implementation:

```
      +-------------------+
      | switch expression |
      +-------------------+

      OP_FALSE
+---  OP_JUMP
|
|     // end     <------------+
|     OP_JUMP  ---------------|---+
|                             |   |
+-->  // case                 |   |
      OP_POP                  |   |
      OP_PUSH                 |   |
                              |   |
      +-------------------+   |   |
      | case expression   |   |   |
      +-------------------+   |   |
                              |   |
      OP_EQUAL                |   |
+---  OP_JUMP_IF_FALSE        |   |
|     OP_POP                  |   |
|                             |   |
|     +-------------------+   |   |
|     | statements        |   |   |
|     +-------------------+   |   |
|                             |   |
|     OP_LOOP  ---------------+   |
|                             |   |
+-->  // case                 |   |
      OP_POP                  |   |
      ...                     |   |
+---  ...                     |   |
|     ...                     |   |
|     OP_LOOP  ---------------+   |
|                                 |
+-->  OP_POP                      |
                                  |
      // default                  |
      +-------------------+       |
      | statements        |       |
      +-------------------+       |
                                  |
      OP_POP  <-------------------+

```
