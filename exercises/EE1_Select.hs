{- HLINT ignore "Use camelCase" -}
module EE1_Select where

import Data.Coerce (coerce)
import Data.Text (Text)
import Database.Esqueleto.Experimental
import Schema
import Types

{-
Congratulations on becoming the new proprietor of the local Stonecold's ice
cream parlor franchise. I'm from the corporate office and will just have a few
questions as you're getting started. Since I'm more on the business side, I
know SQL well...but Haskell is a bridge too far.

Using esqueleto, can you get me everything we know about our flavors?

The equivalent SQL would be:
SELECT * FROM flavors;

If you get stuck, you might want to read the hint in hints/EE1_allFlavors.md

Once you're done, make sure to compare with the answer in answers/EE1_allFlavors.hs
to see if there's any difference
-}
a_allFlavors :: DB [Entity Flavor]
a_allFlavors = do
  _

{-
Actually I just want the flavor name values. That would be:
SELECT flavors.name FROM flavors;

Ensure you do this flavor->name projection in SQL, not after the fact in Haskell.
-}
b_allFlavorNameValues :: DB [Value Text]
b_allFlavorNameValues = do
  _

{-
Both queries above return lists of wrapped types. 'Entity' comes from persistent,
and can be unwrapped into its components via 'entityKey' and 'entityVal'.

'Value' comes from esqueleto. Can you remove the 'Value' wrapper to return a
plain '[Text]'? Start by copying the previous query.
-}
c_allFlavorNames :: DB [Text]
c_allFlavorNames = do
  _

{-
Let's introduce WHERE clauses.
A vegan just walked in. Provide all our dairy-free flavors.
-}
d_dairyFreeFlavors :: DB [Entity Flavor]
d_dairyFreeFlavors = do
  _

{-
It's often convenient to look up FlavorIds from flavor names. For example:

neapolitan <- runDB $ flavorIdsFromNames ["Chunky Chocolate", "Variegated Vanilla", "Smooth Strawberry"]

Write a query that can take an argument of a list of flavor names, and get their IDs
-}
e_flavorIdsFromNames :: [Text] -> DB [FlavorId]
e_flavorIdsFromNames flavorNames = do
  _

{-
We'd like to run a mildly nefarious targeted ad campaign. What are the emails
of all our customers who haven't provided their birthday, but have
provided a favorite flavor?

Fill in the type as well.
-}
f_customersWithoutBirthdaysWithFlavors :: _
f_customersWithoutBirthdaysWithFlavors = do
  _
