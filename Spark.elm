module Spark where

import Array (..)
import Dict
import List (..)
import Maybe

import Option (..)

type Log = { action : Action, reaction : Action, event : Event, outcome : Outcome, effects : [Effect], encounter : Encounter, advance : Bool }

-- Energy Costs: -2, -1, +1, +2?
data Action = Attack | Flee | Befriend | Ignore
data Event = Fight | YouFlee | TheyFlee | EventIgnore | Friendship
data Effect = Damage Int | Healing Int | GetItem Item | BreakItem
data Outcome = Victory | Defeat | Gift | Nada

type Mood = { name : String, aggression : Int, friendliness : Int, fear : Int, neutrality : Int, perception : Int }

type Item = String
type Inventory = [Item]

-- Stats: -1 fight, -1 perceive, +2 befriend, +2 flee; +2 fight, -1 befriend, -1 perceive; +1 fight, +1 flee; +2 perceive, +1 befriend?
data LifeCycle = Child | Adolescent | Adult | Elderly
type Form = { name : String, offense : Int, defense : Int, charm : Int, perception : Int, advance : Option Bool }

type EltType = { name : String,
                 childForm : Form,
                 adolescentForm : Form,
                 adultForm : Form,
                 elderlyForm : Form
               }

type Elt = { elt : EltType, 
             level : LifeCycle,
             inventory : Inventory, 
             health : Int, 
             energy : Int,
             mood : Mood
           }

form : Elt -> Form
form elt = case elt.level of
             Child -> elt.elt.childForm
             Adolescent -> elt.elt.adolescentForm
             Adult -> elt.elt.adultForm
             Elderly -> elt.elt.elderlyForm

offense : Elt -> Int
offense elt = (form elt).offense + elt.mood.aggression

defense : Elt -> Int
defense elt = (form elt).defense + elt.mood.fear

charm : Elt -> Int
charm elt = (form elt).charm + elt.mood.friendliness

perception : Elt -> Int
perception elt = (form elt).perception + elt.mood.perception

setMood : Mood -> Elt -> Elt
setMood mood elt = { elt | mood <- mood }

nextLevel : Elt -> LifeCycle
nextLevel elt = case elt.level of
                  Child -> Adolescent
                  Adolescent -> Adult
                  Adult -> Elderly
                  Elderly -> Elderly

type Entity =
    { name : String,

      mood : Mood,
      deception : Int,
      deceptiveMood : Maybe Mood,

      inventory : Inventory,

      aggression : Int,
      friendliness : Int,
      neutrality : Int,
      fear : Int,

      offense : Int,
      defense : Int,

      gift : Option [Effect],
      victory : Option [Effect],
      defeat : Option [Effect]

    }

aggression : Entity -> Int
aggression entity = entity.aggression + entity.mood.aggression

friendliness : Entity -> Int
friendliness entity = entity.friendliness + entity.mood.friendliness

neutrality : Entity -> Int
neutrality entity = entity.neutrality + entity.mood.neutrality

fear : Entity -> Int
fear entity = entity.neutrality + entity.mood.neutrality

type Location = String

type Encounter = { entity : Entity, location : Location }

type GameState = { elt : Elt, encounter : Encounter, log : Maybe Log }


energyCost : Action -> Int
energyCost command = case command of
                       Attack -> 2
                       Flee -> 1
                       Befriend -> -1
                       Ignore -> -2

canUseAction : Action -> Elt -> Bool
canUseAction command e = e.energy - energyCost command >= 0

payCost : Action -> Elt -> Elt
payCost command e = { e | energy <- clamp 0 10 e.energy - energyCost command }

damage : Int -> Elt -> Elt
damage i e = { e | health <- clamp 0 10 (e.health - i) }

heal : Int -> Elt -> Elt
heal i e = { e | health <- clamp 0 10 (e.health + i) }

getItem : Item -> Elt -> Elt
getItem i e = { e | inventory <- i :: e.inventory }

breakItem : Elt -> Elt
breakItem e = case e.inventory of
                [] -> e
                (item::items) -> { e | inventory <- items }

update : Action -> GameState -> GameState
update c g = g

tempUpdate : Action -> Elt -> Elt
tempUpdate c e = e

fightAggression : Int
fightAggression = 3

-- Should entity reaction depend no player action?
-- There's a reasonable case that it should not...
-- Although I like that things can run away from you when you attack, that's nice.
-- And this nicely separates befriending phases.  Well... nicely?  Eh.
rollReaction : GameState -> Action -> Choice -> Action
rollReaction {elt, encounter} command = 
    let entity = encounter.entity
        opt = intOption <| case command of
                             Attack -> [(fear entity, Flee), (aggression entity, Attack), (neutrality entity, Ignore)]
                             Flee -> [(aggression entity, Attack), (fear entity + neutrality entity + friendliness entity, Ignore)]
                             Befriend -> [(aggression entity, Attack), (fear entity + neutrality entity, Ignore), (friendliness entity, Befriend)]
                             Ignore -> [(aggression entity, Attack), (neutrality entity + fear entity + friendliness entity, Ignore)]
    in
      pickWithDefault Ignore opt

rollEvent : GameState -> Action -> Action -> Choice -> Event
rollEvent {elt, encounter} command action choice = 
    let entity = encounter.entity in
    case (command, action) of
      (Attack, Flee) -> pickWithDefault Fight (intOption [(offense elt, Fight), (entity.defense, TheyFlee)]) choice
      (Attack, _) -> Fight
      (Flee, Attack) -> pickWithDefault Fight (intOption [(defense elt, YouFlee), (entity.defense, Fight)]) choice
      (Flee, _) -> YouFlee
      (Befriend, Befriend) -> Friendship
      (Befriend, Ignore) -> pickWithDefault EventIgnore (intOption [(charm elt, Friendship), (entity.aggression + entity.neutrality + entity.fear, EventIgnore), (entity.friendliness, Friendship)]) choice
      (Befriend, Attack) -> Fight
      (Befriend, Flee) -> TheyFlee
      (Ignore, Attack) -> Fight
      (Ignore, Flee) -> TheyFlee
      (Ignore, Befriend) -> EventIgnore
      (Ignore, Ignore) -> EventIgnore

rollOutcome : GameState -> Event -> Choice -> Outcome
rollOutcome {elt, encounter} event choice =
    let entity = encounter.entity
        location = encounter.location
    in
      case event of
        Fight -> pickWithDefault Victory (intOption [(offense elt, Victory), (entity.offense, Defeat)]) choice
        Friendship -> Gift
        EventIgnore -> Nada
        YouFlee -> Nada
        TheyFlee -> Nada

rollEffects : GameState -> Outcome -> Choice -> [Effect]
rollEffects {elt, encounter} outcome =
    let entity = encounter.entity
        opts = case outcome of
                 Victory -> entity.defeat
                 Defeat -> entity.victory
                 Gift -> entity.gift
                 Nada -> []
    in
      pickWithDefault [] opts

processEffect : Effect -> GameState -> GameState
processEffect event game = 
    case event of
      Damage quantity -> { game | elt <- damage quantity game.elt }
      Healing quantity -> { game | elt <- heal quantity game.elt }
      GetItem item -> { game | elt <- getItem item game.elt }
      BreakItem -> { game | elt <- breakItem game.elt }

processEffects : [Effect] -> GameState -> GameState
processEffects = flip (foldr processEffect)

type Rolls = { reaction : Choice, event : Choice, outcome : Choice, effects : Choice, lifeCycle : Choice}

makeRolls : Float -> Float -> Float -> Float -> Float -> Rolls
makeRolls f1 f2 f3 f4 f5 = { reaction = f1, event = f2, outcome = f3, effects = f4, lifeCycle = f5 }

tick : (Action, Rolls, Encounter, Mood) -> GameState -> GameState
tick (action, rolls, enc, mood) game =
    let reaction = rollReaction game action rolls.reaction
        event = rollEvent game action reaction rolls.event
        outcome = rollOutcome game event rolls.outcome
        effects = rollEffects game outcome rolls.effects
        state = processEffects effects game
        elt = state.elt
        advance = pickWithDefault False (form elt).advance rolls.lifeCycle
        elt' = if advance then { elt | level <- nextLevel elt } else elt
    in
      { state | elt <- setMood mood elt', encounter <- enc, log <- Just { action=action, reaction=reaction, event=event, outcome=outcome, effects=effects,encounter=game.encounter, advance=False } }

over : GameState -> Bool
over game = game.elt.health == 0