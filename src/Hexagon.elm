module Game where
import Time exposing ( .. )
import AnimationFrame
import Keyboard
import Window
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Debug

-- MODEL
type State = NewGame | Play | GameOver

type alias Player =
  { angle: Float }

type alias Input =
  { space : Bool
  , dir : Int
  }

type alias Game =
  { 
    player : Player
  , state : State
  , progress : Int
  , timeStart : Time
  , timeTick : Time
  , msRunning : Float
  }

(gameWidth, gameHeight) = (1024, 576) -- 16:9
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
(iHalfWidth, iHalfHeight) = (gameWidth//2, gameHeight//2)

playerRadius : Float
playerRadius = gameWidth / 10.0

-- The global game state

defaultGame : Game
defaultGame =
  { 
    player = Player (degrees 30)
  , state = NewGame
  , progress = 0
  , timeStart = 0.0
  , timeTick = 0.0
  , msRunning = 0.0
  }

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

updateState: Input -> Game -> State
updateState input game =
  case game.state of
    NewGame -> Play
    Play -> Play
    GameOver -> NewGame

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

updatePlayer: Input -> Game -> Player
updatePlayer {dir} {player} =
  let
    newAngle = updatePlayerAngle player.angle dir
  in
    { player | angle = newAngle }


-- Game loop: Transition from one state to the next.
update : (Time, Input) -> Game -> Game
update (timestamp, input) game =
  { game |
      player = updatePlayer input game
    , state =  Debug.watch "state" (updateState input game)
    , progress = Debug.watch "progress" (updateProgress game)
    , timeStart = Debug.watch "timeStart" (if game.state == NewGame then timestamp else game.timeStart)
    , timeTick = timestamp
    , msRunning = Debug.watch "msRunning" (updateMsRunning timestamp game)
  }

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

view : (Int,Int) -> Game -> Element
view (w, h) game =
  container w h middle <|
  collage gameWidth gameHeight
    [ rect gameWidth gameHeight
        |> filled bgBlack
    , makePlayer game.player
    ]

-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions gameState

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input

-- Creates an event stream from the keyboard inputs and is
-- updated by AnimationFrame.
input : Signal (Time, Input)
input =
  Signal.map2 Input
    Keyboard.space
    (Signal.map .x Keyboard.arrows)
  -- only update on a new frame
  |> Signal.sampleOn AnimationFrame.frame
  |> Time.timestamp

