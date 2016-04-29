# Lazy Forms for Common Lisp

This is a simple lazy form evaluation package for Common Lisp.

## Quickstart

A lazy form (or [thunk](https://en.wikipedia.org/wiki/Thunk)) is simply a function wrapping a body of code that will only every be executed once, and the result remembered for future use. 

To create a lazy form use the `lazy` macro:

    (lazy &body form)  ;=> THUNK

Let's try it:

    CL-USER > (lazy (+ 1 2))
    #<THUNK UNREALIZED>

Let's resolve it with the `lazy-value` function:

    CL-USER > (lazy-value *)
    3

Try it again, but with something that has side-effects:

    CL-USER > (lazy (print 'hi) (+ 1 2))
    #<THUNK UNREALIZED>

Resolve it and get the value:

    CL-USER > (lazy-value *)
    HI
    3

Get the value again, and notice that the thunk isn't executed a second time:

    CL-USER > (lazy-value **)
    3

That's it!