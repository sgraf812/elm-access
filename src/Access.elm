module Access
    ( Accessor, Accessor'
    , accessor
    , get, set, update
    , compose, (@.)
    , allExamplesOk'_'
    ) where


{-| # Access

This module has the primitives which make working with accessors a cake walk:

    atIndex = accessor -- Let's pretend for the moment this exists
    foo = accessor .foo (\newFoo x -> { x | foo <- newFoo })

    manyFoos = [{ foo = { bar = 1 } }, { foo = { bar = 42 } }]

    manyFoos |> set (atIndex 1 @. foo @. bar) 20 == [{ foo = 1 }, { foo = 20 }]
    manyFoos |> get (atIndex 1 @. foo) == { bar = 1 }

Notice how not only getting values out of a deep structure is now easily possible but also that updating them is now as easy!
As evident by the hypothetical `atIndex` accessor, this can be extended far beyond simple field access.

I will provide compatible accessors in my [`Graph`](https://github.com/sgraf812/elm-graph) library, *without actually depending on this library*, just because it is such a hassle to provide
getters and setter for all use cases when all that is needed are some simple accessors which compose well through `@.`.

## Type aliases
@docs Accessor, Accessors'
## Make your accessor
@docs accessor, compose, (@.)
## Access patterns
@docs get, set, update

-}


import Lazy exposing (Lazy)
import Debug
    
{-| Uhoh, here we are. Let's first make sense of the 4 type parameters! And because it's all easier with an example, study this record:
 
    type alias HasFoo a = { foo : a }

Notice how `HasFoo` is polymorphic in `a`. Nothing forbids having both `{ foo = 1 }`
(of type `HasFoo Int` that is) and `{ foo = "1" }` (of type `HasFoo String`)! In fact,
that is the sole reason we have 4 type parameters instead of just 2 (that would be the simpler version `Accessor'`).
In the following we consider how we would arrange types to update the inner `foo` to its `toString` representation.

Now to stab (ha) the type parameters once and for all:

  - `s` - Think `HasFoo Int`: The type of structure we start with. The s is for *source*.
  - `t` - Think `HasFoo String`: The type of structure we end up with. For the scenario of getters, this is always the same as `s`. The t is for *target*.
  - `a` - Think `Int`, the type of the field: The type of the part of the structure we start with.
  - `b` - Think `String`: The type of the part we end up with.

So with that inplace, let's talk about the right hand side, the function we actually alias with this!
Firstly, **understanding and liking this is not necessary**. It is an abstraction mechanism
with which you only really come in touch when you provide your own complex accessors and don't want to
depend on functions like `accessor` which have a much simpler interface.

**The reason why this is an alias rather than a new abstract data type is to give library writers the opportunity to
offer accessors without actually depending on this library, by just providing such a matching function.**

Without further ado, I'll try to get your intuition working: An `Accessor` is a function transforming an operation
on *part* of the structure into an operation on the *whole* of that structure:

    type alias Accessor s t a b =
           (a -> (a, b))
        -> (s -> (a, t))

    type alias FooAccessor =
           (Int -> (Int, String))
        -> (HasFoo Int -> (Int, HasFoo String))

(I added a set of parantheses and left out the `Lazy` stuff, which just complicates matters for now)

Consider for a moment just the `fst` part of each pair:

    (a -> a)     -> (s  -> a)
    (Int -> Int) -> (HasFoo Int -> Int)
                                ^ .foo

When squinting a little, we can already see the part of the getter `s -> a`.
Now, if we got such a function, we can just apply it to `identity` (which has the handy type `a -> a`)
and get a hand on our getter!

Focusing on to the `snd` part of each pair, we get:

    (a -> b)        -> (s -> t)
    (Int -> String) -> (HasFoo Int -> HasFoo String)

So, by passing it a function that updates part of the structure, we get back a function that updates the structure as a whole.
Notice that only here we used types `b` and `t` (which may even collapse to the same types as `a` and `s`).

**TLDR** `Accessor`s are functions which combine getters and setters into one rather daunting thing
which exhibits nicer properties to work with that just getters and setters.
-}
type alias Accessor s t a b = (a -> (a, Lazy b)) -> s -> (a, Lazy t)

{-| A type alias for the frequenst case when source and target type are the same thing. -}
type alias Accessor' s a = Accessor s s a a


foo : Accessor { anything | foo : a } { anything | foo : b } a b
foo = accessor .foo (\newFoo x -> { x | foo <- newFoo })
bar = accessor .bar (\newBar x -> { x | bar <- newBar })

{-| `accessor get set` constructs an accessor from a getter `get` and setter `set`.

    foo : Accessor { anything | foo : a } { anything | foo : b } a b
    foo = accessor .foo (\newFoo x) -> { x | foo <- newFoo })

Because of the way Elm's extensible records work, we just defined an accessor for the foo
field of all extensible records containing them.
-}
accessor : (s -> a) -> (b -> s -> t) -> Accessor s t a b
accessor get set f s =
    let (a, lb) = f (get s)
        lt = Lazy.lazy <| \_ -> set (Lazy.force lb) s
    in (a, lt)


ex1 : Bool
ex1 =
    let x = { foo = 1 }
    in get foo x == x.foo

{-| Get a part (type `a`) of the structure (type `s`) with the help of the accessor.
So if `foo` would be an `Accessor` which extracts e.g. the `foo` field of the record `x`,
we get `x.foo` by

    let x = { foo = 1 }
    in get foo x == x.foo
-} 
get : Accessor s t a b -> s -> a 
get acc = acc (\a -> (a, Lazy.lazy (\_ -> Debug.crash "Access.get: This lazy value should never be forced."))) >> fst


ex2 : Bool
ex2 =
    let x = { foo = 1 }
    in set foo "hi" x == { x | foo <- "hi" }

{-| Set a part (type `a`, `b`) of the structure (type `s`, `t`) with the help of the accessor.
It's easiest to see an example of that:

    let x = { foo = 1 }
    in set foo "hi" x == { x | foo <- "hi" }

Note the analogy to the built-in record update syntax.
-}
set : Accessor s t a b -> b -> s -> t
set l b = update l (always b)

          
ex3 : Bool
ex3 =
    let x = { foo = 1 }
    in update foo toString x == { x | foo <- toString x.foo } -- x.foo == "1"

{-| Update a part (type `a`, `b`) of the structure (type `s`, `t`) with the help of the accessor.
Example:

    let x = { foo = 1 }
    in update foo toString x == { x | foo <- toString x.foo } -- x.foo == "1"

Note the analogy to the built-in record update syntax.
-}
update : Accessor s t a b -> (a -> b) -> s -> t
update l f = l (\a -> (a, a |> f |> always |> Lazy.lazy)) >> snd >> Lazy.force
        

ex4 : Bool
ex4 =
    let x = { foo = { bar = 1 } }
    in set (foo `compose` bar) 2 x == { foo = { bar = 2 } }

{-| Composes two accessors (backwards), so that deep access patterns are possible.
Example on record fields:

    let x = { foo = { bar = 1 } }
    in set (foo `compose` bar) 2 x == { foo = { bar = 2 } }

See also the composition operator `@.`. 
-}
compose : Accessor u v s t -> Accessor s t a b -> Accessor u v a b
compose outer inner = 
    let get' = get outer >> get inner
        set' b u = set outer (set inner b (get outer u)) u
    in accessor get' set'


ex5 : Bool
ex5 =
    let x = { foo = { bar = 1 } }
    in set (foo@.bar) 2 x == { foo = { bar = 2 } }

{-| An infix synonym for `compose`. The same example using infix notation:

    let x = { foo = { bar = 1 } }
    in set (foo@.bar) 2 x == { foo = { bar = 2 } }

The syntax was carefully chosen to mirror record field access. 
You can even use the infix synonym `^.` for `get` to get particular syntactic frauds.
Although that may need some getting used to.
-}
(@.) = compose


ex6 : Bool
ex6 =
    let x = { foo = { bar = 1 } }
    in x^.foo@.bar == 1

{-| An infix synonym for `get`.

    let x = { foo = { bar = 1 } }
    in x^.foo@.bar == 1

-}
(^.) val acc = get acc val

infixl 7 ^.
infixl 8 @.

allExamplesOk'_' : Bool
allExamplesOk'_' = ex1 && ex2 && ex3 && ex4 && ex5 && ex6