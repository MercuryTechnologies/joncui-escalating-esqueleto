{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module EE3_TypeZoo where

import Control.Monad.IO.Class (MonadIO)
import Database.Esqueleto.Experimental
import Database.Esqueleto.Experimental.From (ToFrom)
import Database.Esqueleto.Experimental.From.Join (HasOnClause)
import Database.Esqueleto.Internal.Internal (SqlSelect)
import Types
import Control.Monad.Reader (ReaderT)

{-
The goal of this module is to get you comfortable with some of the many types used in the esqueleto library,
and some relevant types from outside of it (e.g. `DB` and the function `runDB`).

You could figure out many of these instantly with hoogle or by running the typechecker.
Try to rely on the hints in hints/EE3*.md instead.
The goal is fluency, not getting through as quickly as possible.

--

In lib/Types.hs we define:

type DB = SqlPersistT (LoggingT IO)

(There's some additional commentary there about why this specific version of `DB` isn't very safe.)

This DB monad matches up with the return type of esqueleto's `select` and similar functions.

--

*** For the exercises below, fill in any holes in the types (leave the values as `undefined`). ***
-}

{-
runDB is a function that takes a DB action and actually runs it on the database
-}
a_runDB :: DB a -> IO a
a_runDB = undefined

{-
There are a few main types it's nice to get familiar with at the outset.

`SqlQuery` helps us build up our queries.
In Haskell, it's a monad, which is why you'll often see queries starting with `select $ do`
Many parts of the query builder language return a `SqlQuery ()`, e.g. `where_`, `on`, and `groupBy`.

`SqlExpr` represents a "smaller" SQL expression.
For example, `where_` and `on` each take `SqlExpr (Value Bool)`s as arguments.
`^.` and `?.` return `SqlExpr`s.
`val` takes a plain type and lifts it into a `SqlExpr`.
`SqlExpr` is notoriously _not_ a Functor. You cannot `fmap` over a `SqlExpr`!
You may have run into this before when trying to `unValue` in the wrong place.

`Entity` comes from Persistent and represents a whole table, along with a primary key (id) for that table.

`Value` comes from Esqueleto and represents a single field, or any standalone value.

`PersistField` shuttles types between their database-storage versions, and their Haskell versions.

Finally, `:&` helps us construct joins. Its precedence is to the left.
-}

{-
`select` returns a `ReaderT backend m` action (which is the same as `DB`) to grab us a list of rows, and takes an argument...which is up to you to figure out
-}
b_select :: (SqlSelect a r, MonadIO m, SqlBackendCanRead backend) => SqlQuery a -> ReaderT backend m [r]
b_select = undefined

{-
`val` takes any plain Haskell type which is an instance of `PersistField` and lifts it into Esqueleto land. What's its type?
-}
c_val :: PersistField typ => typ -> SqlExpr typ
c_val = undefined

{-
Given these types for `asc` and `desc`:

asc :: PersistField a => SqlExpr (Value a) -> SqlExpr OrderBy
desc :: PersistField a => SqlExpr (Value a) -> SqlExpr OrderBy

And these example usages:

orderBy [asc flavor.name]
orderBy [asc foo, desc bar, asc baz]
orderBy [desc (countRows :: SqlExpr (Value Int))]

What's the type of `orderBy`?
-}
d_orderBy :: [SqlExpr OrderBy] -> SqlQuery ()
d_orderBy = undefined

{-
The (^.) and (?.) projection operators take us from an `Entity` to a `Value` (both wrapped in `SqlExpr`), by using an `EntityField`.

Note that if you're using OverloadedRecordDot, `x.id` could refer to either `x ^. #id` or `x ?. #id`.
This happens because there are `HasField` instances for both operators, the Maybe case and the plain case. The types keep track of whether the Maybe is around.

The type of (^.) is:

(^.) :: forall typ val. (PersistEntity val, PersistField typ) => SqlExpr (Entity val) -> EntityField val typ -> SqlExpr (Value typ)

Given that, can you introduce `Maybe`s in the right places to produce the correct type for the (?.) operator? The `EntityField` argument type is unchanged.

The answer/hint are named `EE3e_project` instead of `EE3e_(?.)`
-}
(?.) :: (PersistEntity val, PersistField typ) => SqlExpr (Maybe (Entity val)) -> EntityField val typ -> SqlExpr (Value (Maybe typ))
(?.) = undefined

{-
The various types of joins are intimately connected with nullability and the `Maybe` type.

Given the types of `innerJoin` and `fullOuterJoin`, can you complete the type of a `leftJoin`?
This might be intimidating! Try to break down the syntax into smaller parts, and match them up.

innerJoin :: (ToFrom a a', ToFrom b b', HasOnClause rhs (a' :& b'), rhs ~ (b, (a' :& b') -> SqlExpr (Value Bool))) => a -> rhs -> From (a' :& b')
fullOuterJoin :: (ToFrom a a', ToFrom b b', ToMaybe a', ToMaybe b', HasOnClause rhs (ToMaybeT a' :& ToMaybeT b'), rhs ~ (b, (ToMaybeT a' :& ToMaybeT b') -> SqlExpr (Value Bool))) => a -> rhs -> From (ToMaybeT a' :& ToMaybeT b')

Uncomment the type signature to get started
-}

f_leftJoin :: (ToFrom a a', ToFrom b b', ToMaybe b', HasOnClause rhs (a' :& ToMaybeT b'), rhs ~ (b, (a' :& ToMaybeT b') -> SqlExpr (Value Bool))) => a -> rhs -> From (a' :& ToMaybeT b')
f_leftJoin = undefined
