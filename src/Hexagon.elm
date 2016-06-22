import Time exposing (..)
import List exposing (..)
import AnimationFrame
import Keyboard.Extra as Keyboard
import Window
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)

import Debug
import Text
import String exposing (padLeft)

import Html.App as App
import Html


-- MODEL
type State = NewGame | Play | GameOver | Pause

type Msg
  = Step Time
  | KeyboardExtraMsg Keyboard.Msg


type alias Player =
  { angle: Float }

type Direction = Left | Right | Still

type alias Enemy =
  { radius : Float
  , parts : List(Bool)
  }

type alias Game =
  {
    player : Player
  , direction : Direction
  , enemies: List(Enemy)
  , enemySpeed: Float
  , keyboardModel : Keyboard.Model
  , state : State
  , timeStart : Time
  , timeTick : Time
  , msRunning : Float
  , autoRotateAngle : Float
  , autoRotateSpeed : Float
  }

type alias Colors =
  { dark : Color
  , medium: Color
  , bright : Color
  }


(gameWidth, gameHeight) = (1024, 576) -- 16:9
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
(iHalfWidth, iHalfHeight) = (gameWidth//2, gameHeight//2)

playerRadius : Float
playerRadius = gameWidth / 10.0

enemyThickness = 30
enemyDistance = 350

enemies = 
  [ [False, True, False, True, False, True]
  , [False, True, True, True, True, True]
  , [True, False, True, True, True, True]
  , [True, True, True, True, False, True]
  , [True, True, True, False, True, True]
  , [False, True, False, True, False, True]
  , [True, False, True, False, True, False]
  , [True, True, True, True, True, False]
  ]

startMessage = "SPACE to start, &larr;&rarr; to move"


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


colidesWith: Player -> Enemy -> Bool
colidesWith player enemy =
  let 
    collidesAtIndex: Int -> Bool
    collidesAtIndex index = 
      let 
        fromAngle = (toFloat index) * 60
        toAngle = ((toFloat index)+1)*60
        playerDegrees = player.angle * 360 / (2*pi)
      in
        playerDegrees >= fromAngle && playerDegrees < toAngle
  in
    if enemy.radius > playerRadius || enemy.radius + enemyThickness < playerRadius then
      False
    else
      -- check if open

        indexedMap (,) enemy.parts |> filter snd |> map fst |> any collidesAtIndex

updatePlayer: Direction -> Game -> Player
updatePlayer dir {player, state} =
  if state == Play then
    let
      newAngle = if state == NewGame then degrees 30
                 else updatePlayerAngle player.angle dir
    in
      { player | angle = newAngle }
  else
    player


isGameOver: Game -> Bool
isGameOver {player, enemies} =
  any (colidesWith player) enemies


updateMsRunning: Time -> Game -> Time
updateMsRunning timestamp game =
  case game.state of
    Play -> game.msRunning + timestamp - game.timeTick
    NewGame -> 0.0
    _ -> game.msRunning

updateAutoRotateAngle: Game -> Float
updateAutoRotateAngle {autoRotateAngle, autoRotateSpeed} =
  autoRotateAngle + autoRotateSpeed

updateAutoRotateSpeed: Game -> Float
updateAutoRotateSpeed {msRunning, autoRotateSpeed} =
  0.02 * sin (msRunning * 0.0003 |> Debug.log "φ")
  |> Debug.log "autoRotateSpeed"


updateEnemies: Game -> List(Enemy)
updateEnemies game =
  let
    enemyProgress = game.msRunning * game.enemySpeed
    numEnemies = List.length enemies
    maxDistance = numEnemies * enemyDistance

    offsetForEnemy index = 
      round <| enemyDistance * (toFloat index) - enemyProgress

    radiusFor index = 
      (offsetForEnemy index) % maxDistance
      |> toFloat 
  in
    List.indexedMap (\index parts -> {
      parts = parts, 
      radius = radiusFor index
    }) enemies


updateEnemySpeed: Game -> Float
updateEnemySpeed game =
  Debug.log "enemy speed" (0.15 + game.msRunning/500000)

{-| Updates the game state on a keyboard command -}
onUserInput : Keyboard.Msg -> Game -> (Game, Cmd Msg)
onUserInput keyMsg game =

  let
    ( keyboardModel, keyboardCmd ) =
      Keyboard.update keyMsg game.keyboardModel
    spacebar = Keyboard.isPressed Keyboard.Space keyboardModel &&
      not (Keyboard.isPressed Keyboard.Space game.keyboardModel)
    direction =
      if (Keyboard.arrows keyboardModel).x < 0 then Left
      else if (Keyboard.arrows keyboardModel).x > 0 then Right
      else Still
    nextState =
      case game.state of
        NewGame -> if spacebar then Play else NewGame
        Play -> if spacebar then Pause else Play
        GameOver -> if spacebar then NewGame else GameOver
        Pause -> if spacebar then Play else Pause
  in
    ( { game | keyboardModel = keyboardModel
             , direction = direction
             , state = nextState
      }
    , Cmd.map KeyboardExtraMsg keyboardCmd )

{-| Updates the game state on every frame -}
onFrame : Time -> Game -> (Game, Cmd Msg)
onFrame time game =
  let
    nextCmd = Cmd.none
    nextState =
      case game.state of
        NewGame -> NewGame
        Play -> if isGameOver game then GameOver else Play
        _ -> game.state
  in
    ( { game |
        player = updatePlayer game.direction game
      , enemies = updateEnemies game
      , enemySpeed = updateEnemySpeed game
      , state = Debug.log "state" nextState
      , timeStart = if game.state == NewGame then time else game.timeStart
      , timeTick = time
      , msRunning = Debug.log "msRunning" (updateMsRunning time game)
      , autoRotateAngle = updateAutoRotateAngle game
      , autoRotateSpeed = updateAutoRotateSpeed game
    }, nextCmd )


{-| Game loop: Transition from one state to the next. -}
update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    KeyboardExtraMsg keyMsg -> onUserInput keyMsg game
    Step time -> onFrame time game


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

trapezoid: Float -> Float -> Color -> Form
trapezoid base height color =
  let
    s = height/(tan <| degrees 60)
  in
    filled color <| polygon [
      (-base/2, 0), (base/2, 0), (base/2-s, height), (-base/2+s, height)
    ]


makeEnemy : Color -> Enemy -> Form
makeEnemy color enemy =
  let
    base = 2.0 * (enemy.radius +enemyThickness) / (sqrt 3)
    makeEnemyPart : Int -> Form
    makeEnemyPart index =
      trapezoid base enemyThickness color
        |> rotate (degrees <| toFloat (90 + index * 60))
        |> moveRadial (degrees <| toFloat (index * 60)) (enemy.radius +enemyThickness)

    -- color = (hsl (radius/100) 1 0.5)
  in
    group
      (indexedMap (,) enemy.parts |> filter snd |> map fst |> map makeEnemyPart)

makeEnemies : Color -> List(Enemy) -> List(Form)
makeEnemies color enemys =
  map (makeEnemy color) enemys



hexagonElement: Int -> List((Float, Float))
hexagonElement i =
  let
    radius = halfWidth * sqrt 2
    angle0 = 60 * i |> toFloat |> degrees
    angle1 = 60 * (i+1) |> toFloat |> degrees
  in
    [(0.0, 0.0)
    , (sin angle0 * radius, cos angle0 * radius)
    , (sin angle1 * radius, cos angle1 * radius)
    ]

makeField: Colors -> Form
makeField colors =
  let
    color i =
      if i % 2 == 0 then
        colors.dark
      else
        colors.medium
    poly i =
      polygon (hexagonElement i)
      |> filled (color i)
  in
    group (map poly [0..5])

-- the polygon in the center: this is just decoration, so it has no own state
makeCenterHole : Colors -> Game -> List Form
makeCenterHole colors game =
  let
    shape = ngon 6 60
    line = solid colors.bright
  in
    [ shape
        |> filled colors.dark
        |> rotate (degrees 90)
    , shape
        |> (outlined {line | width = 4.0})
        |> rotate (degrees 90)
    ]

makeColors : Float -> Colors
makeColors msRunning =
  let
    hue = degrees 0.01 * (toFloat <| round msRunning % 36000)
  in
    { dark = (hsl hue 0.6 0.2)
    , medium = (hsl hue 0.6 0.3)
    , bright = (hsla hue 0.6 0.6 0.8)
    }

makeTextBox : Float -> String -> Element
makeTextBox size string =
  Text.fromString string
    |> Text.color (rgb 255 255 255)
    |> Text.monospace
    |> Text.height size
    |> leftAligned

formatTime : Time -> String
formatTime running =
  let
    centiseconds = floor (Time.inMilliseconds running / 10)
    seconds = centiseconds // 100
    centis = centiseconds % 100
  in
    padLeft 3 '0' (toString seconds) ++ "." ++ padLeft 2 '0' (toString centis)


view : Game -> Html.Html Msg
view game =
  let
    bg = rect gameWidth gameHeight |> filled bgBlack

    colors = makeColors game.msRunning
    score =
      formatTime game.msRunning
      |> makeTextBox 50
    message = makeTextBox 50 <|
      case game.state of
        GameOver -> "Game Over"
        Pause -> "Pause"
        _ -> ""
    field = append
        [ makeField colors
        , makePlayer game.player
        , group <| makeEnemies colors.bright game.enemies
        ]
        (makeCenterHole colors game)
      |> group
  in
    toHtml <|
    container gameWidth gameHeight middle <|
    collage gameWidth gameHeight
      [ bg
      , field |> rotate game.autoRotateAngle
      , toForm message |> move (0, 40)
      , toForm score |> move (100 - halfWidth, halfHeight - 40)
      , toForm (
          if game.state == Play then spacer 1 1
          else makeTextBox 20 startMessage
        ) |> move (0, 40 - halfHeight)
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
      , state = NewGame
      , enemies = []
      , enemySpeed = 0.0
      , timeStart = 0.0
      , timeTick = 0.0
      , msRunning = 0.0
      , autoRotateAngle = 0.0
      , autoRotateSpeed = 0.0
      }
    , Cmd.map KeyboardExtraMsg keyboardCmd
    )



main =
  App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions }

