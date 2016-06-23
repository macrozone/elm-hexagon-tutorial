import Time exposing (..)
import List exposing (..)
import AnimationFrame
import Keyboard.Extra as Keyboard
import Window
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Html.App as App
import Html

-- MODEL

type alias Player =
  { angle: Float }

type Direction = Left | Right | Still

type alias Game =
  {
    player : Player
  , direction : Direction
  , keyboardModel : Keyboard.Model
  }

type Msg
  = Step Time
  | KeyboardExtraMsg Keyboard.Msg


(gameWidth, gameHeight) = (1024, 576) -- 16:9
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
(iHalfWidth, iHalfHeight) = (gameWidth//2, gameHeight//2)

playerRadius : Float
playerRadius = gameWidth / 10.0

bgBlack : Color
bgBlack =
  rgb 20 20 20


-- UPDATE

updatePlayerAngle: Float -> Direction -> Float
updatePlayerAngle angle dir =
  let
    sign =
      if dir == Left then 1
      else if dir == Right then -1
      else 0
    newAngle = (angle + toFloat (sign * 4) * 0.032)
  in
    if newAngle < 0 then
      newAngle + 2 * pi
    else if newAngle > 2 * pi then
      newAngle - 2 * pi
    else
      newAngle

updatePlayer: Direction -> Game -> Player
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
    direction =
      if (Keyboard.arrows keyboardModel).x < 0 then Left
      else if (Keyboard.arrows keyboardModel).x > 0 then Right
      else Still
  in
    ( { game | keyboardModel = keyboardModel
             , direction = direction
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
    }, nextCmd )


{-| Game loop: Transition from one state to the next. -}
update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    KeyboardExtraMsg keyMsg -> onUserInput keyMsg game
    Step time -> onFrame time game


-- VIEW

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
      , direction = Still
      }
    , Cmd.map KeyboardExtraMsg keyboardCmd
    )


main =
  App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions }

