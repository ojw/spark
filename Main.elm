module Main where

import Graphics.Input
import Window
import Random

import Option (..)
import UI (..)
import Spark (..)
import World (..)

main : Signal Element
main = render' <~ state ~ Window.dimensions

render' : GameState -> (Int,Int) -> Element
render' game window = if over game then plainText "You died." else render game window

initialState : GameState
initialState = { elt = newElt spark, encounter = { location = "wherever", entity = goblin }, log = Nothing }

testRender : GameState -> (Int, Int) -> Rolls -> Element
testRender gs xy r = asText r `above` render gs xy

rollRender : Rolls -> Element
rollRender rolls = asText rolls `above` flow right [attackButton, fleeButton, befriendButton, ignoreButton]

action : Signal Action
action = merges [attack.signal, flee.signal, befriend.signal, ignore.signal]

rolls : Signal Rolls
rolls = makeRolls <~ Random.float action ~ Random.float action ~ Random.float action ~ Random.float action ~ Random.float action

location : Signal Location
location = pickWithDefault "nowhere" locations <~ Random.float action

entity : Signal Entity
entity = pickWithDefault goblin entities <~ Random.float action

setEntityMood : Mood -> Entity -> Entity
setEntityMood mood entity = { entity | mood <- mood }

entityMood : Signal Mood
entityMood = pickWithDefault neutral moods <~ Random.float action

eltMood : Signal Mood
eltMood = pickWithDefault neutral moods <~ Random.float action

encounter : Signal Encounter
encounter = (\ent loc -> { entity = ent, location = loc }) <~ lift2 setEntityMood entityMood entity ~ location

are : Signal (Action, Rolls, Encounter, Mood)
are = (\ a r e m -> (a,r,e,m)) <~ action ~ rolls ~ encounter ~ eltMood

state : Signal GameState
state = foldp tick initialState are

