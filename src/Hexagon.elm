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
import Audio exposing (PlaybackOptions, defaultPlaybackOptions, Sound)

import String exposing (padLeft)

import Html.App as App
import Html
import Task exposing (Task, andThen)


-- MODEL
type State = Loading | NewGame | Starting | Play | Pausing | Pause | Resume | GameOver

type Msg
  = Step Time
  | KeyboardExtraMsg Keyboard.Msg
  | MusicLoaded Sound
  | Error String
  | Noop


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
  , progress : Int
  , timeStart : Time
  , timeTick : Time
  , msRunning : Float
  , autoRotateAngle : Float
  , autoRotateSpeed : Float
  , hasBass : Bool
  , music: Maybe Sound
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
startMessage = "SPACE to start, &larr;&rarr; to move"


beat = 138.0 |> bpm
beatAmplitude = 0.06
beatPhase = 270 |> degrees

-- Calculate Beat Per Minute
bpm : Float -> Float
bpm beat =
  (2.0 * pi * beat / 3600 )

pump : Int -> Float
pump progress = beatAmplitude * (beat * toFloat progress + beatPhase |> sin)

-- MUSIC

hasBass : Time -> Bool
hasBass time =
  if time < 20894 then False
  else if time < 41976 then True
  else if time < 55672 then False
  else if time < 67842 then True
  else if time < 187846 then False
  else if time < 215938 then True
  else False


loadSound : Task String Sound
loadSound = Audio.loadSound "music/shinytech.mp3"


playbackOptions = {
  defaultPlaybackOptions | loop = True, startAt = Nothing }

playSound : Sound -> PlaybackOptions -> Cmd Msg
playSound sound options =
  Task.perform Error (always Noop) <| Audio.playSound options sound

stopSound : Sound -> Cmd Msg
stopSound sound =
  Task.perform (always Noop) (always Noop) <| Audio.stopSound sound



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

updateAutoRotateAngle: Game -> Float
updateAutoRotateAngle {autoRotateAngle, autoRotateSpeed} =
  autoRotateAngle + autoRotateSpeed

updateAutoRotateSpeed: Game -> Float
updateAutoRotateSpeed {progress, autoRotateSpeed} =
  0.02 * sin (toFloat progress * 0.005)

updateEnemies: Game -> List(Enemy)
updateEnemies game =
  let
    enemyDistance = 300
    enemies =
      [ [False, True, False, True, False, True]
      , [True, True, True, False, True, True]
      , [False, True, False, True, True, True]
      , [False, True, True, True, True, True]
      , [True, False, True, True, True, True]
      , [True, False, True, True, True, False]
      , [False, False, False, True, True, True]
      , [False, True, True, True, True, True]
      ]
    numEnemies = List.length enemies
    maxDistance = numEnemies * enemyDistance
    enemyProgress = (toFloat game.progress) * game.enemySpeed
    radiusFor index =
      enemyThickness + (iHalfWidth + round ( enemyDistance * (toFloat index) - enemyProgress)) % maxDistance
      |> toFloat
  in
    List.indexedMap (\index parts -> {
      parts = parts,
      radius = radiusFor index
    }) enemies


updateEnemySpeed: Game -> Float
updateEnemySpeed game =
  2 + (toFloat game.progress)/1000


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
        NewGame -> if spacebar then Starting else NewGame
        Play -> if spacebar then Pausing else Play
        GameOver -> if spacebar then NewGame else GameOver
        Pause -> if spacebar then Resume else Pause
        _ -> game.state
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
    (nextState, nextCmd) =
      case game.music of
        Nothing -> (Loading, Cmd.none)
        Just music ->
          case game.state of
            Starting -> (Play, playSound music { playbackOptions | startAt = Just 0 })
            Resume -> (Play, playSound music playbackOptions)
            Pausing -> (Pause, stopSound music)
            Play -> if isGameOver game
              then (GameOver, stopSound music)
              else (Play, Cmd.none)
            _ -> (game.state, Cmd.none)
  in
    ( { game |
        player = updatePlayer game.direction game
      , enemies = updateEnemies game
      , enemySpeed = updateEnemySpeed game
      , state =  nextState
      , progress = updateProgress game
      , timeStart = if game.state == Starting then time else game.timeStart
      , timeTick = time
      , msRunning = updateMsRunning time game
      , autoRotateAngle = updateAutoRotateAngle game
      , autoRotateSpeed = updateAutoRotateSpeed game
      , hasBass = hasBass game.msRunning
    }, nextCmd )


{-| Game loop: Transition from one state to the next. -}
update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    KeyboardExtraMsg keyMsg -> onUserInput keyMsg game
    Step time -> onFrame time game
    MusicLoaded music ->
      ( { game |
          state = NewGame,
          music = Just music
        }, Cmd.none)
    Error message -> Debug.crash message
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
    bassAdd = if game.hasBass then
        100.0 * beatAmplitude
      else
        100.0 * beatAmplitude * (beat * toFloat game.progress |> sin)
    shape = ngon 6 (60 + bassAdd)
    line = solid colors.bright
  in
    [ shape
        |> filled colors.dark
        |> rotate (degrees 90)
    , shape
        |> (outlined {line | width = 4.0})
        |> rotate (degrees 90)
    ]

makeColors : Int -> Colors
makeColors progress =
  let
    hue = degrees 0.1 * (toFloat <| progress % 3600)
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

beatPulse : Game -> Form -> Form
beatPulse game =
  if game.hasBass then
    scale (1 + beatAmplitude * (beat * toFloat game.progress |> sin))
  else
    identity

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
    colors = makeColors game.progress
    score =
      formatTime game.msRunning
      |> makeTextBox 50
    message = makeTextBox 50 <|
      case game.state of
        Loading -> "Loading..."
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
      , field |> rotate game.autoRotateAngle |> beatPulse game
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
      , progress = 0
      , timeStart = 0.0
      , timeTick = 0.0
      , msRunning = 0.0
      , autoRotateAngle = 0.0
      , autoRotateSpeed = 0.0
      , hasBass = False
      , music = Nothing
      }
    , Cmd.batch
      [ Cmd.map KeyboardExtraMsg keyboardCmd
      , Task.perform Error MusicLoaded loadSound
      ]
    )



main =
  App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions }
