{- HLINT ignore "Use camelCase" -}
module EE2_Join where

import Data.Coerce (coerce)
import Data.Text (Text)
import Database.Esqueleto.Experimental
import Database.Esqueleto.PostgreSQL
import Schema
import Types

{-
What are all our customers' favorite flavors?

If they don't have one, give back a `Nothing`.
-}
a_favoriteFlavors :: DB [(Entity Customer, Maybe (Entity Flavor))]
a_favoriteFlavors =
  select $ do
    (c :& f) <- from $ table @Customer
      `leftJoin` table @Flavor
        `on` do \(c :& f) -> c.favoriteFlavor ==. f.id
    pure (c, f)
  

{-
We'd like to determine the popularity of each flavor.

Return a list of each flavor name along with how many customers have it as
their favorite flavor. Sort it by popularity in descending order,
and then alphabetically by name.

Sample results:
[ ("Chunky Chocolate", 27)
, ("Smooth Strawberry", 12)
, ("Coconut Cream", 3)
, ("Variegated Vanilla", 3)
]
-}
b_flavorPopularity :: DB [(Text, Int)]
b_flavorPopularity = do
  res <- select $ do
    (f :& _) <- from $ table @Flavor
      `innerJoin` table @Customer
        `on` do \(f :& c) -> just f.id ==. c.favoriteFlavor
    groupBy_ f.name
    orderBy [desc (countRows :: SqlExpr (Value Int)), asc f.name]
    pure (f.name, countRows)

  pure $ coerce (res :: [(Value Text, Value Int)])

{-
We have a concept of "groups" provided by CustomerLink and CustomerGroupParent.

Who are all the customers in the largest group?
-}
c_largestGroup :: DB [Entity Customer]
c_largestGroup = do
  let
    parentId =
      subSelect $ do
        cl <- from $ table @CustomerLink
        groupBy_ cl.parentId
        orderBy [desc (countRows :: SqlExpr (Value Int))]
        limit 1
        pure cl.parentId

  select $ do
    (c :& cl) <- from $ table @Customer
      `innerJoin` table @CustomerLink
        `on` do \(c :& cl) -> c.id ==. cl.customerId
    where_ $ just cl.parentId ==. parentId
    pure c

  

{-
For each CustomerGroupParent, list its ID as well as the IDs of all the customers in that group.
-}
d_customerGroups :: DB [(CustomerGroupParentId, [CustomerId])]
d_customerGroups = do
  res <- select $ do
    (g :& _ :& c) <- from $
      table @CustomerGroupParent
      `innerJoin` table @CustomerLink
        `on` do \(g :& cl) -> g.id ==. cl.parentId
      `innerJoin` table @Customer
        `on` do \(_ :& cl :& c) -> cl.customerId ==. c.id
    groupBy_ g.id
    pure (g.id, maybeArray $ arrayAgg c.id)

  pure $ coerce (res :: [(Value CustomerGroupParentId, Value [CustomerId])])
