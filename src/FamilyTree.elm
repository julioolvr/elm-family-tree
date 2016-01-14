module FamilyTree (Person(BasePerson, Child), NationalityRatio, nationalityRatio) where

{-| Utilities for modeling and querying a family tree.

# Definition
@docs Person, nationalityRatio, NationalityRatio
-}

import String
import Dict
-- import Html exposing (text)

type alias Country =
  String


{-| Represents a person on the family tree. It can either be a base person which has
their own country, or a child with a mother and a father.
-}
type Person =
  BasePerson { name : String, country : Country }
  | Child { name : String } Person Person


{-| Represents how much of a nationality a person is.
-}
type alias NationalityRatio =
  (Country, Float)


{-| Cuts the ratio by half.

    halveRatio ("Italy", 0.5) == ("Italy", 0.25)
-}
halveRatio : NationalityRatio -> NationalityRatio
halveRatio (country, ratio) =
  (country, ratio / 2)

{-| Groups the ratios by country. If there are several ratios for the same country,
they are added together.

    groupRatios [("Spain", 0.25), ("Italy", 0.5), ("Spain", 0.25)] == [("Spain", 0.5), ("Italy", 0.5)]
-}
groupRatios : List (String, Float) -> List (String, Float)
groupRatios ratios =
  let
    updateRatio ratio previousRatio =
      case previousRatio of
        Just r ->
          Just(ratio + r)
        Nothing ->
          Just ratio

    addToDict (country, ratio) dict =
      Dict.update country (updateRatio ratio) dict
  in
    Dict.toList <| List.foldr addToDict Dict.empty ratios


{-| Given a person it tells you how much of each nationality they are.

    father : Person
    father = BasePerson { name = "Father", country = "Italy" }

    mother : Person
    mother = BasePerson { name = "Mother", country = "Spain" }

    child : Person
    child = Child { name = "Child" } father mother

    nationalityRatio child == [("Italy", 0.5), ("Spain", 0.5)]
-}
nationalityRatio : Person -> List NationalityRatio
nationalityRatio person =
  case person of
    BasePerson { country } ->
      [(country, 1.0)]
    Child _ father mother ->
      List.append (nationalityRatio father) (nationalityRatio mother)
        |> List.map halveRatio
        |> groupRatios

-- From here on it will go away, it's just to test it on http://elm-lang.org/try

-- grandfather1 : Person
-- grandfather1 = BasePerson { name = "Grandfather 1", country = "Italy" }
--
-- grandmother1 : Person
-- grandmother1 = BasePerson { name = "Grandmother 1", country = "Spain" }
--
-- grandfather2 : Person
-- grandfather2 = BasePerson { name = "Grandfather 2", country = "France" }
--
-- grandmother2 : Person
-- grandmother2 = BasePerson { name = "Grandmother 2", country = "Italy" }
--
-- father : Person
-- father = Child { name = "Father" } grandfather1 grandmother1
--
-- mother : Person
-- mother = Child { name = "Mother" } grandfather2 grandmother2
--
-- child : Person
-- child = Child { name = "Child" } father mother
--
-- main =
--   text <| toString <| nationalityRatio child
