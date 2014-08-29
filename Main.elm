module Main where

import Graphics.Input
import Window
import Random

import Option (..)
import UI (..)
import Spark (..)
import World (..)

main : Signal Element
--main = tempRender <~ rolls
main = testRender <~ state ~ Window.dimensions ~ rolls

initialState : GameState
initialState = { elt = newElt spark, encounter = { location = "wherever", entity = goblin } }

testRender : GameState -> (Int, Int) -> Rolls -> Element
testRender gs xy r = asText r `above` render gs xy

action : Signal Action
action = merges [attack.signal, flee.signal, befriend.signal, ignore.signal]

rolls : Signal Rolls
rolls = makeRolls <~ Random.float action ~ Random.float action ~ Random.float action ~ Random.float action

location : Signal Location
location = pickWithDefault "nowhere" locations <~ Random.float action

entity : Signal Entity
entity = pickWithDefault goblin entities <~ Random.float action

encounter : Signal Encounter
encounter = (\ent loc -> { entity = ent, location = loc }) <~ entity ~ location

are : Signal (Action, Rolls, Encounter)
are = (\ a r e -> (a,r,e)) <~ action ~ rolls ~ encounter

state : Signal GameState
state = foldp tick initialState are