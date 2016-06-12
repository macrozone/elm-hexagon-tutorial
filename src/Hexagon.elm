import Time exposing ( .. )
import AnimationFrame
import Keyboard.Extra as Keyboard
import Window
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Html
import Html.App as App

-- MODEL

type alias Player =
  { angle: Float }


type Msg
  = Init Time
  | Step Time
  | Pause
  | KeyboardExtraMsg Keyboard.Msg

type alias Game =
  {
    player : Player,
    direction : Int,
    keyboardModel : Keyboard.Model
  }

(gameWidth, gameHeight) = (1024, 576) -- 16:9
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
(iHalfWidth, iHalfHeight) = (gameWidth//2, gameHeight//2)

playerRadius : Float
playerRadius = gameWidth / 10.0



-- UPDATE

updatePlayerAngle : Float -> Int -> Float
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

updatePlayer: Int -> Game -> Player
updatePlayer dir {player} =
  let
    newAngle = updatePlayerAngle player.angle dir
  in
    { player | angle = newAngle }

-- Game loop: Transition from one state to the next.
update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    KeyboardExtraMsg keyMsg ->
      let
        ( keyboardModel, keyboardCmd ) =
          Keyboard.update keyMsg game.keyboardModel
      in
        ( { game | keyboardModel = keyboardModel
                 , direction = (Keyboard.arrows keyboardModel).x
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )
    Step time -> ({game | player = updatePlayer game.direction game }, Cmd.none)
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
  toHtml <|
  container gameWidth gameHeight middle <|
  collage gameWidth gameHeight
    [ rect gameWidth gameHeight
        |> filled bgBlack
    , makePlayer game.player
    ]


-- SUBSCRIPTIONS

step : Time -> Msg
step time =
  Step time


subscriptions : Game -> Sub Msg
subscriptions game =
  Sub.batch [
    AnimationFrame.times step,
    Sub.map KeyboardExtraMsg Keyboard.subscriptions
  ]



--INIT

init : (Game, Cmd Msg)
init =
  let
    ( keyboardModel, keyboardCmd ) = Keyboard.init
  in
    ( { player = Player (degrees 30),
        keyboardModel = keyboardModel,
        direction = 0
      },
      Cmd.map KeyboardExtraMsg keyboardCmd
    )



main =
  App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions }
