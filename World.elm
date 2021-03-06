module World where

import Array
import Spark (..)
import Dict
import List

import Option (..)

locations : Option Location
locations = normalizeOption [(1, "in a forest"), 
                             (1, "in a meadow"),
                             (1, "by a river"),
                             (1, "on a mountain"), 
                             (1, "in a town"), 
                             (1, "on a road")]

energyWords : Dict.Dict Int String
energyWords = Dict.fromList [(1, "Exhausted"), 
                             (2, "Exhausted"),
                             (3, "Tired"),
                             (4, "Tired"),
                             (5, "Normal"),
                             (6, "Normal"),
                             (7, "Energetic"),
                             (8, "Energetic"),
                             (9, "Manic"),
                             (10, "Manic")]

spark : EltType
spark = { name = "spark",
          childForm      = { name = "spark", offense = 2, defense = 3, charm = 4, perception = 3, advance = normalizeOption [(1, True), (100, False)] },
          adolescentForm = { name = "flame", offense = 4, defense = 3, charm = 1, perception = 2, advance = normalizeOption [(1, True), (200, False)] },
          adultForm      = { name = "fire",  offense = 3, defense = 2, charm = 2, perception = 3, advance = normalizeOption [(1, True), (500, False)] },
          elderlyForm    = { name = "ember", offense = 2, defense = 2, charm = 2, perception = 3, advance = normalizeOption [(1, True)] }
        }

newElt : EltType -> Elt
newElt eltType = { elt = eltType,
                   level = Child,
                   inventory = [],
                   health = 10,
                   energy = 10,
                   mood = neutral
                 }

neutral : Mood
neutral = { name = "neutral", aggression = 0, friendliness = 0, fear = 0, neutrality = 0, perception = 0 }

surly : Mood
surly = { name = "surly", aggression = 1, friendliness = 0, fear = 1, neutrality = 0, perception = 0 }

friendly : Mood
friendly = { name = "friendly", aggression = 0, friendliness = 2, fear = 0, neutrality = 0, perception = 0 }

moods = normalizeOption [(1,neutral), (1,surly), (1, friendly)]

baseEntity : Entity
baseEntity = { name = "a vague blob", mood = neutral, deception = 0, deceptiveMood = Nothing, inventory = [],
               aggression = 0, friendliness = 1, neutrality = 0, fear = 0,
               offense = 1, defense = 1,
               gift = [], victory = [], defeat = []
             }

worm : Entity
worm = { name = "worm",
         mood = neutral,
         deception = 0,
         deceptiveMood = Nothing,
         inventory = [],
         aggression = 2,
         friendliness = 0,
         neutrality = 3,
         fear = 0,
         offense = 1,
         defense = 0,
         gift = [],
         victory = [(1, [Damage 1])],
         defeat = []
       }

goblin : Entity
goblin = { name = "goblin",
           mood = surly,
           deception = 0,
           deceptiveMood = Nothing,
           inventory = [],
           aggression = 2,
           friendliness = 0,
           neutrality = 1,
           fear = 1,
           offense = 2,
           defense = 1,
           gift = [(1, [GetItem "some gross ball of lint"])],
           victory = [(1, [Damage 1])],
           defeat = []
         }

elf : Entity
elf = { name = "elf",
        mood = friendly,
        deception = 0,
        deceptiveMood = Nothing,
        inventory = [],
        aggression = 1,
        friendliness = 4,
        neutrality = 1,
        fear = 1,
        offense = 1,
        defense = 1,
        gift = [(1, [Healing 1])],
        victory = [(1, [Damage 1])],
        defeat = []
      }

entities : Option Entity
entities = normalizeOption [(1, goblin), (1, elf), (1,worm)]
                  

healthWords : Dict.Dict Int String
healthWords = Dict.fromList <| List.zip [0..11] ["dead", "approaching death", "horribly mangled", "severely wounded", "badly hurt", "hurt", "banged up", "a little bruised", "okay", "just fine", "quite healthy"]