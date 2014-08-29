module UI where

import Graphics.Input (..)
import Window

import Spark (..)
import World (..)


render : GameState -> (Int, Int) -> Element
render game (x,y) = container x y midTop <|
    flow down [renderElt game.elt, 
               renderLocation game.encounter.location,
               renderEntity game.encounter.entity,
               flow right [attackButton, fleeButton, befriendButton, ignoreButton],
               renderLogs game
              ]

renderElt : Elt -> Element
renderElt elt = flow down [plainText ("You are a " ++ (form elt).name ++ "."),
                           plainText ("You feel " ++ elt.mood.name ++ ".")]

renderLocation : Location -> Element
renderLocation loc = plainText ("You are " ++ loc)

renderEntity : Entity -> Element
renderEntity ent = flow down [plainText ("You see a " ++ ent.name ++ "."),
                              plainText ("It looks " ++ ent.mood.name ++ ".")]

renderAction : Action -> GameState -> Element
renderAction action game = let entity = game.encounter.entity.name in
                           case action of
                             Attack -> plainText ("You attack the " ++ entity ++ "!")
                             Flee -> plainText ("You run away from the " ++ entity ++ "!")
                             Befriend -> plainText ("You try befriending the " ++ entity ++ ".")
                             Ignore -> plainText ("You play it cool.")

renderReaction : Action -> Action -> GameState -> Element
renderReaction action reaction game =let entity = game.encounter.entity.name in
                                     case (action, reaction) of
                                       (Attack, Attack) -> plainText ("The " ++ entity ++ " charges back at you!")
                                       (Attack, Flee) -> plainText ("The " ++ entity ++ " turns and tries to run!")
                                       (Attack, Befriend) -> plainText ("The " ++ entity ++ " tries to say something.")
                                       (Attack, Ignore) -> plainText ("The " ++ entity ++ " stands its ground.")
                                       (Flee, Attack) -> plainText ("The " ++ entity ++ " gives chase!")
                                       (Flee, Flee) -> plainText ("The " ++ entity ++ " runs the other way, just as fast!")
                                       (Flee, Befriend) -> plainText ("The " ++ entity ++ " waves and calls \"Hey wait!\"")
                                       (Flee, Ignore) -> plainText ("The " ++ entity ++ " kinda just ignores you.")
                                       (Befriend, Attack) -> plainText ("The " ++ entity ++ " attacks you by surprise!")
                                       (Befriend, Flee) -> plainText ("As you try to speak, the " ++ entity ++ " turns and flees.")
                                       (Befriend, Befriend) -> plainText ("The " ++ entity ++ " is totally digging you.")
                                       (Befriend, Ignore) -> plainText ("The " ++ entity ++ " listens to what you say.")
                                       (Ignore, Attack) -> plainText ("The " ++ entity ++ " decides to attack!")
                                       (Ignore, Flee) -> plainText ("The " ++ entity ++ " doesn't like the looks of you and runs away!")
                                       (Ignore, Befriend) -> plainText ("The " ++ entity ++ " comes over and is friendly.")
                                       (Ignore, Ignore) -> plainText ("You and the " ++ entity ++ " just ignore each other.")

renderEvent : Event -> GameState -> Element
renderEvent event game = let entity = game.encounter.entity.name in
                         case event of
                           Fight -> plainText "You fight!"
                           YouFlee -> plainText "You get away safely."
                           TheyFlee -> plainText ("The " ++ entity ++ " gets away!")
                           EventIgnore -> plainText "Nothing really happens."
                           Friendship -> plainText ("You and the " ++ entity ++ " become pals!")

renderOutcome : Outcome -> GameState -> Element
renderOutcome outcome game = let entity = game.encounter.entity.name in
                             case outcome of
                               Victory -> plainText ("You defeat the " ++ entity ++ "!")
                               Defeat -> plainText ("The " ++ entity ++ " puts a beatdown on you!")
                               Gift -> plainText ("The " ++ entity ++ " gives you a gift!")
                               Nada -> plainText ("Nothing happens at all.")

renderEffect : GameState -> Effect -> Element
renderEffect game effect = let entity = game.encounter.entity.name in
                           case effect of
                             Damage i -> plainText ("You take " ++ show i ++ " damage.")
                             Healing i -> plainText ("You are healed by " ++ show i ++ ".")
                             GetItem i -> plainText ("The " ++ entity ++ " gives you a " ++ show i ++ "!")
                             BreakItem -> plainText ("The " ++ entity ++ " breaks an item in your inventory!")

renderLogs : GameState -> Element
renderLogs game = case game.log of
                    Nothing -> plainText "You know nothing of the world."
                    Just log -> flow down <| [ renderAction log.action game,
                                               renderReaction log.action log.reaction game,
                                               renderEvent log.event game,
                                               renderOutcome log.outcome game ] ++ map (renderEffect game) log.effects

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
