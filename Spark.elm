module Spark where

import Array (..)

data Command = Fight | Flee | Befriend | Ignore

data EltType = Spark | Breeze | Shower | Frost | Dirt

type Elt = { eltType : EltType, names : Array String, defName : String, adjectives : Array String, defAdjective : String }

type Item = String

type Inventory = [Item]

type EltState = { elt : Elt, level : Int, inventory : Inventory}

type GameState = { player : EltState }

type Entity = { name : String, reaction : Command -> (String, EltState -> EltState) }

type Location = String

type Encounter = { entity : Entity, location : Location }

spark : Elt
spark = { eltType = Spark
        , names = fromList ["Spark", "Flame", "Fire", "Inferno"]
        , defName = "Spark"
        , adjectives = fromList ["Glowing", "Flickering", "Dancing", "Raging"]
        , defAdjective = "Glowing"
        }

initialElt : Elt -> EltState
initialElt e = { elt = e, level = 0, inventory = [] }

update : Command -> EltState -> EltState
update c e = { e | level <- e.level + 1 }

goblin : Entity
goblin = { name = "Goblin"
         , reaction = \c -> case c of
                              Fight -> ("You beat up the goblin and level up." , \e -> { e | level <- e.level + 1 })
                              Flee  -> ("You run away, but are now tired.", \e -> { e | level <- min (e.level - 1) 0 })
                              Befriend -> ("The goblin punches you in the gut.", \e -> { e | level <- min (e.level - 1) 0})
                              Ignore -> ("Nothing interesting happens", id)
         }