module Access
    ( Accessor, Accessor'
    , get
    ) where

import Lazy exposing (Lazy)
import Debug
    
type alias Accessor s t a b = (a -> (a, Lazy b)) -> s -> (a, Lazy t)
type alias Accessor' s a = Accessor s s a a


foo_ : Accessor { anything | foo : a } { anything | foo : b } a b
foo_ = accessor .foo (\newFoo x -> { x | foo <- newFoo })

{-| `accessor get set` constructs an accessor from a getter `get` and setter `set`.

    foo_ : Accessor { anything | foo : a } { anything | foo : b } a b
    foo_ = accessor .foo (\newFoo x) -> { x | foo <- newFoo })

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
    in get foo_ x == x.foo

{-| Get a part (type `a`) of the structure (type `s`) with the help of the accessor.
So if `foo_` would be an `Accessor` which extracts e.g. the `foo` field of the record `x`,
we get `x.foo` by

    let x = { foo = 1 }
    in get foo_ x == x.foo
-} 
get : Accessor s t a b -> s -> a 
get acc = acc (\a -> (a, Lazy.lazy (\_ -> Debug.crash "Access.get: This lazy value should never be forced."))) >> fst


ex2 : Bool
ex2 =
    let x = { foo = 1 }
    in set foo_ "hi" x == { x | foo <- "hi" }

{-| Set a part (type `a`, `b`) of the structure (type `s`, `t`) with the help of the accessor.
It's easiest to see an example of that:

    let x = { foo = 1 }
    in set foo_ "hi" x == { x | foo <- "hi" }

Note the analogy to the built-in record update syntax.
-}
set : Accessor s t a b -> b -> s -> t
set l b = update l (always b)

          
ex3 : Bool
ex3 =
    let x = { foo = 1 }
    in update foo_ toString x == { x | foo <- toString x.foo } -- x.foo == "1"

{-| Update a part (type `a`, `b`) of the structure (type `s`, `t`) with the help of the accessor.
Example:

    let x = { foo = 1 }
    in update foo_ toString x == { x | foo <- toString x.foo } -- x.foo == "1"

Note the analogy to the built-in record update syntax.
-}
update : Accessor s t a b -> (a -> b) -> s -> t
update l f = l (\a -> (a, a |> f |> always |> Lazy.lazy)) >> snd >> Lazy.force
        

{-| Composes two accessors (backwards), so that deep access patterns are possible.
Example on record fields:

    let x = { foo = { bar = 1 } }
    in set (bar_ `Access.compose` foo_) 2 x == { foo = { bar = 2 } }

See also the composition operator `@.`. 
-}
compose : Accessor u v s t -> Accessor s t a b -> Accessor u v a b
compose outer inner = 
    let get' = get outer >> get inner
        set' b u = set outer (set inner b (get outer u)) u
    in accessor get' set'
