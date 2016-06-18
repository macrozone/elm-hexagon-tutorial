import Time exposing (..)
import List exposing (..)
import AnimationFrame
import Keyboard.Extra as Keyboard
import Window
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)

import Debug
import Html.App as App
import Html

-- MODEL
type State = NewGame | Play | GameOver | Starting | Pause | Resume



type Msg
  = Step Time
  | KeyboardExtraMsg Keyboard.Msg
  | Noop

type alias Player =
  { angle: Float }

type alias Game =
  { 
    player : Player
  , state : State
  , progress : Int
  , timeStart : Time
  , timeTick : Time
  , msRunning : Float
  , direction : Int
  , keyboardModel : Keyboard.Model
  }

(gameWidth, gameHeight) = (1024, 576) -- 16:9
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
(iHalfWidth, iHalfHeight) = (gameWidth//2, gameHeight//2)

playerRadius : Float
playerRadius = gameWidth / 10.0



-- UPDATE

updatePlayerAngle: Float -> Int -> Float
updatePlayerAngle angle dir =
  let
    newAngle = (angle + toFloat (-dir * 4) * 0.032)
  in
    if newAngle < 0 then
      newAngle + 2 * pi
    else if newAngle > 2 * pi then
      newAngle - 2 * pi
    else
      newAngle



updateProgress: Game -> Int
updateProgress {state,progress} =
  case state of
    NewGame -> 0
    Play -> progress + 1
    _ -> progress

updateMsRunning: Time -> Game -> Time
updateMsRunning timestamp game = 
  case game.state of
    Play -> game.msRunning + timestamp - game.timeTick 
    NewGame -> 0.0
    _ -> game.msRunning

updatePlayer: Int -> Game -> Player
updatePlayer dir {player} =
  let
    newAngle = updatePlayerAngle player.angle dir
  in
    { player | angle = newAngle }

{-| Updates the game state on a keyboard command -}
onUserInput : Keyboard.Msg -> Game -> (Game, Cmd Msg)
onUserInput keyMsg game =
  let
    ( keyboardModel, keyboardCmd ) =
      Keyboard.update keyMsg game.keyboardModel
    spacebar = Keyboard.isPressed Keyboard.Space keyboardModel &&
      not (Keyboard.isPressed Keyboard.Space game.keyboardModel)
    
    nextState =
      case game.state of
        NewGame -> if spacebar then Starting else NewGame
        Play -> if spacebar then Pause else Play
        GameOver -> if spacebar then NewGame else GameOver
        Pause -> if spacebar then Resume else Pause
        _ -> game.state
  in
    ( { game | keyboardModel = keyboardModel
             , direction = (Keyboard.arrows keyboardModel).x
      }
    , Cmd.map KeyboardExtraMsg keyboardCmd )

{-| Updates the game state on every frame -}
onFrame : Time -> Game -> (Game, Cmd Msg)
onFrame time game =
  let
    nextCmd = Cmd.none
  in
    ( { game |
        player = updatePlayer game.direction game
      , state =  Debug.log "state" (updateState input game)
      , progress = Debug.log "progress" (updateProgress game)
      , timeStart = Debug.log "timeStart" (if game.state == NewGame then time else game.timeStart)
      , timeTick = time
      , msRunning = Debug.log "msRunning" (updateMsRunning time game)
    }, nextCmd )


{-| Game loop: Transition from one state to the next. -}
update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    KeyboardExtraMsg keyMsg -> onUserInput keyMsg game
    Step time -> onFrame time game
    _ -> (game, Cmd.none)


-- VIEW

bgBlack : Color
bgBlack =
  rgb 20 20 20

moveRadial : Float -> Float -> Form -> Form
moveRadial angle radius =
  move (radius * cos angle, radius * sin angle)

makePlayer : Player -> Form
makePlayer player =
  let 
    angle = player.angle - degrees 30
  in
    ngon 3 10
      |> filled (hsl angle 1 0.5)
      |> moveRadial angle (playerRadius - 10)
      |> rotate angle

view : Game -> Html.Html Msg
view game =
  let
    bg = rect gameWidth gameHeight |> filled bgBlack
    field = makePlayer game.player
  in
    toHtml <|
    container gameWidth gameHeight middle <|
    collage gameWidth gameHeight
      [ bg
      , field
      ]

-- SUBSCRIPTIONS

subscriptions : Game -> Sub Msg
subscriptions game =
  Sub.batch [
    AnimationFrame.times (\time -> Step time),
    Sub.map KeyboardExtraMsg Keyboard.subscriptions
  ]


--INIT

init : (Game, Cmd Msg)
init =
  let
    ( keyboardModel, keyboardCmd ) = Keyboard.init
  in
    ( { player = Player (degrees 30)
      , keyboardModel = keyboardModel
      , direction = 0
      }
    , Cmd.batch
      [ Cmd.map KeyboardExtraMsg keyboardCmd
      ]
    )


main =
  App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions }

