module UI where

import Graphics.Input (..)
import Window

import Spark (..)

main : Signal Element
main = lift2 (render "You are a spark.") command Window.dimensions

state : Signal EltState
state = foldp update (initialElt spark) command

command : Signal Command
command = merges [fight.signal, flee.signal, befriend.signal, ignore.signal]

commandButton : Handle Command -> Command -> String -> Element
commandButton h c s = button h c s |> color yellow

renderElt : EltState -> Element
renderElt e = plainText ("You are a " ++ e.elt.defName)

render : String -> Command -> (Int, Int) -> Element
render s c (x,y) = container x y middle <| (plainText s `above` asText c `above` flow right [fightButton, fleeButton, befriendButton, ignoreButton])

{- UI Elements -}

fight : Input Command
fight = input Fight

fightButton : Element
fightButton = commandButton fight.handle Fight "Fight"

flee : Input Command
flee = input Flee

fleeButton : Element
fleeButton = commandButton flee.handle Flee "Flee"

befriend : Input Command
befriend = input Befriend

befriendButton : Element
befriendButton = commandButton befriend.handle Befriend "Befriend"

ignore : Input Command
ignore = input Ignore

ignoreButton : Element
ignoreButton = commandButton ignore.handle Ignore "Ignore"
