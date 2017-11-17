effect module Window.Key where { subscription = MySub } exposing
  ( onKeyDown, onKeyUp, onKeyPress
  )

{-| This module lets you listen to global keyboard events using the modern event.key interface,
    with a mirror interface to the Html Attribute interface provided by Key.


# Subscriptions
@docs onKeyDown, onKeyUp, onKeyPress

-}

import Dict
import Dom.LowLevel as Dom
import Process
import Task exposing (Task)

import Key exposing (Key)


-- MOUSE EVENTS


{-| Subscribe to all key presses.
-}
onKeyPress : (Key -> msg) -> Sub msg
onKeyPress tagger =
  subscription (MySub "keypress" tagger)


{-| Subscribe to get codes whenever a key goes down.
-}
onKeyDown : (Key -> msg) -> Sub msg
onKeyDown tagger =
  subscription (MySub "keydown" tagger)


{-| Subscribe to get codes whenever a key goes up.
-}
onKeyUp : (Key -> msg) -> Sub msg
onKeyUp tagger =
  subscription (MySub "keyup" tagger)



-- SUBSCRIPTIONS


type MySub msg
  = MySub String (Key -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub category tagger) =
  MySub category (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
  Dict.Dict String (Watcher msg)


type alias Watcher msg =
  { taggers : List (Key -> msg)
  , pid : Process.Id
  }



-- CATEGORIZE SUBSCRIPTIONS


type alias SubDict msg =
  Dict.Dict String (List (Key -> msg))


categorize : List (MySub msg) -> SubDict msg
categorize subs =
  categorizeHelp subs Dict.empty


categorizeHelp : List (MySub msg) -> SubDict msg -> SubDict msg
categorizeHelp subs subDict =
  case subs of
    [] ->
      subDict

    MySub category tagger :: rest ->
      categorizeHelp rest <|
        Dict.update category (categorizeHelpHelp tagger) subDict


categorizeHelpHelp : a -> Maybe (List a) -> Maybe (List a)
categorizeHelpHelp value maybeValues =
  case maybeValues of
    Nothing ->
      Just [value]

    Just values ->
      Just (value :: values)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
  Task.succeed Dict.empty


type alias Msg =
  { category : String
  , key      : Key
  }


(&>) task1 task2 =
  Task.andThen (\_ -> task2) task1


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
  let
    leftStep category {pid} task =
      Process.kill pid &> task

    bothStep category {pid} taggers task =
      Task.map (Dict.insert category (Watcher taggers pid)) task

    rightStep category taggers task =
      task
        |> Task.andThen (\state -> Process.spawn (Dom.onDocument category Key.key (Platform.sendToSelf router << Msg category))
        |> Task.andThen (\pid -> Task.succeed (Dict.insert category (Watcher taggers pid) state)))
  in
    Dict.merge
      leftStep
      bothStep
      rightStep
      oldState
      (categorize newSubs)
      (Task.succeed Dict.empty)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router {category, key} state =
  case Dict.get category state of
    Nothing ->
      Task.succeed state

    Just {taggers} ->
      let
        send tagger =
          Platform.sendToApp router (tagger key)
      in
        Task.sequence (List.map send taggers)
          |> Task.andThen (\_ -> Task.succeed state)
