# UGLY/HACKY/RETARDED SHIT
## HOW THE FUCK DOES THISREF WORK GOD HELP
Every property access, in the current context, set `ctx.current_instance` to the result of the access.

When we do any sort of destruction (I.E. in a function call), unset `ctx.current_instance` (set to `None`).

When doing a function call, there's an ugly hack that takes place. Because new() doesn't inherit its parent's `current_instance`, we have to manually copy it over.

I AM AWARE THIS IS HACKY AND UGLY AND BAD

Oh this... exists...
```rust
                    /* we want to UNSET the current_instance after any
                     * destructions, we don't want that to linger around.
                     * 
                     * THIS IS A VERY, EXTREMELY RETARDED WAY OF DOING THIS!
                     * the "elegant" solution is to create a new context for
                     * every property access, with current_instance being set
                     * accordingly, but that is a lot of allocations, and
                     * allocs don't grow on trees (unfortunately...)
                     */
                    b.current_instance.take();
```

# CLARIFICATIONS/WEIRD NON-SELF-DOCUMENTING STUFF
## Expr::SomethingLiteral
These are expressions that, when evaluated, turn into the corresponding `Value::Literal(Value::Something(...))` type.

## Expr::Property
The point is that this returns an `Rc<RefCell<Value>>` for use in other expressions.

The left-hand side is called the a_value and is expected to be an expression.
The right-hand side is called the b_key, it is expected to be an expression that evaluates into an ID (`.into_id()`-compatible).

## skip_ws()
CALL THIS FUCKING EVERYWHERE THERE CAN BE A SPACE OR A COMMENT OR SOMETHING LIKE THAT

## current_instance
this is the thing used for thisrefs

## binary op table
