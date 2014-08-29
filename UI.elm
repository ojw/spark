module UI where

import Graphics.Input (..)
import Window

import Spark (..)
import World (..)


render : GameState -> (Int, Int) -> Element
render {elt, encounter} (x,y) = container x y middle <|
    flow down [renderElt elt, 
               renderLocation encounter.location,
               renderEntity encounter.entity,
               flow right [attackButton, fleeButton, befriendButton, ignoreButton]
              ]

renderElt : Elt -> Element
renderElt elt = flow down [plainText ("You are a " ++ (form elt).name ++ "."),
                           plainText ("You feel " ++ elt.mood.name ++ ".")]

renderLocation : Location -> Element
renderLocation loc = plainText ("You are " ++ loc)

renderEntity : Entity -> Element
renderEntity ent = flow down [plainText ("You see a " ++ ent.name ++ "."),
                              plainText ("It looks " ++ ent.mood.name ++ ".")]



{- UI Elements -}

actionButton : Handle Action -> Action -> String -> Element
actionButton h c s = button h c s |> color yellow

attack : Input Action
attack = input Attack

attackButton : Element
attackButton = actionButton attack.handle Attack "Attack"

flee : Input Action
flee = input Flee

fleeButton : Element
fleeButton = actionButton flee.handle Flee "Flee"

befriend : Input Action
befriend = input Befriend

befriendButton : Element
befriendButton = actionButton befriend.handle Befriend "Befriend"

ignore : Input Action
ignore = input Ignore

ignoreButton : Element
ignoreButton = actionButton ignore.handle Ignore "Ignore"
