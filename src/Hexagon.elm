module Game where
import Time exposing (..)
import List exposing (..)
import AnimationFrame
import Keyboard
import Window
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Debug
import Text
import Audio
import Music
import String exposing (padLeft)

-- MODEL
type State = NewGame | Starting | Play | Pause | GameOver

type alias Player =
  { angle: Float }

type alias Enemy = 
  { radius : Float
  , parts : List(Bool)
  }

type alias Input =
  { space : Bool
  , dir : Int
  }


type alias Game =
  { 
    player : Player
  , enemies: List(Enemy)
  , enemySpeed: Float
  , state : State
  , progress : Int
  , timeStart : Time
  , timeTick : Time
  , msRunning : Float
  , autoRotateAngle : Float
  , autoRotateSpeed : Float
  , hasBass : Bool
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

beat = 120.0 |> bpm
beatAmplitude = 0.06

-- Calculate Beat Per Minute
bpm : Float -> Float
bpm beat =
  (2.0 * pi * beat / 3600 )

handleAudio : Game -> Audio.Action
handleAudio game =
  case game.state of
    Play -> Audio.Play
    Starting -> Audio.Seek 0
    _ -> Audio.Pause

propertiesHandler : Audio.Properties -> Maybe Audio.Action
propertiesHandler properties = Nothing

music : Signal (Audio.Event, Audio.Properties)
music = Audio.audio { src = "music/music.mp3",
                      triggers = Audio.defaultTriggers,
                      propertiesHandler = propertiesHandler,
                      actions = Signal.map handleAudio gameState }

-- The global game state

defaultGame : Game
defaultGame =
  { 
    player = Player (degrees 30)
  , enemies = []
  , enemySpeed = 0.0
  , state = NewGame
  , progress = 0
  , timeStart = 0.0
  , timeTick = 0.0
  , msRunning = 0.0
  , autoRotateAngle = 0.0
  , autoRotateSpeed = 0.0
  , hasBass = False
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

colidesWith: Player -> Enemy -> Bool
colidesWith player enemy =
  let 
    collidesAtIndex: Int -> Bool
    collidesAtIndex index = 
      let 
        fromAngle = Debug.watch ("from Angle"++toString index) ((toFloat index) * 60) 
        toAngle = Debug.watch ("to Angle"++ toString index) (((toFloat index)+1)*60)
        playerDegrees = Debug.watch "player degrees" (player.angle * 360 / (2*pi))
      in
        playerDegrees >= fromAngle && playerDegrees < toAngle
  in
    if enemy.radius > playerRadius || enemy.radius + enemyThickness < playerRadius then
      False
    else
      -- check if open

        indexedMap (,) enemy.parts |> filter snd |> map fst |> any collidesAtIndex



isGameOver: Game -> Bool
isGameOver {player, enemies} =
  any (colidesWith player) enemies

updateState: Input -> Game -> State
updateState input game =
  case game.state of
    Starting -> Play
    NewGame -> if input.space then Starting else Pause
    Play -> 
      if input.space then Pause else 
        if isGameOver game then GameOver else Play
    Pause -> if input.space then Play else Pause
    GameOver -> if input.space then NewGame else GameOver


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
  0.02 * sin (toFloat progress * 0.005 |> Debug.watch "φ")
  |> Debug.watch "autoRotateSpeed"

updatePlayer: Input -> Game -> Player
updatePlayer {dir} {player, state} =
  if state == Play then
    let
      newAngle = if state == NewGame then degrees 30 else 
        Debug.watch "Player angle" (updatePlayerAngle player.angle dir)
    in
      { player | angle = newAngle }
  else
    player

updateEnemies: Game -> List(Enemy)
updateEnemies game =
  let
    enemyDistance = 300
    partsFor index = 
      case index of
        0 -> [True, True, True, False, True, True]
        1 -> [True, True, True, False, True, True]
        2 -> [False, True, False, True, True, True]
        3 -> [False, True, True, True, True, True]
        _ -> [True, False, True, True, True, True]
    radiusFor index = 
      toFloat (enemyThickness + (iHalfWidth + round (( enemyDistance * (toFloat index)) - (toFloat game.progress) * game.enemySpeed)) % (enemyDistance * 5))
  in
   [
      {parts = partsFor 0, radius = radiusFor 0}
    , {parts = partsFor 1, radius = radiusFor 1}
    , {parts = partsFor 2, radius = radiusFor 2}
    , {parts = partsFor 3, radius = radiusFor 3}
    , {parts = partsFor 4, radius = radiusFor 4}
    ]

updateEnemySpeed: Game -> Float
updateEnemySpeed game = 
  Debug.watch "enemy speed" (2 + (toFloat game.progress)/1000)

-- Game loop: Transition from one state to the next.
update : (Time, Input) -> Game -> Game
update (timestamp, input) game =
  { game |
      player = updatePlayer input game
    , enemies = updateEnemies game
    , enemySpeed = updateEnemySpeed game
    , state =  Debug.watch "state" (updateState input game)
    , progress = Debug.watch "progress" (updateProgress game)
    , timeStart = Debug.watch "timeStart" (if game.state == NewGame then timestamp else game.timeStart)
    , timeTick = timestamp
    , msRunning = Debug.watch "msRunning" (updateMsRunning timestamp game)
    , autoRotateAngle = updateAutoRotateAngle game
    , autoRotateSpeed = updateAutoRotateSpeed game
    , hasBass = Debug.watch "hasBass" (Music.hasBass game.msRunning)
       
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

makeTextBox : (Text.Text -> Text.Text) -> String -> Element
makeTextBox f string =
  Text.fromString string
    |> Text.color (rgb 255 255 255)
    |> Text.monospace
    |> f
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


view : (Int,Int) -> Game -> Element
view (w, h) game =
  let
    colors = makeColors game.progress
    startMessage = "SPACE to start, &larr;&rarr; to move"
    score =
      formatTime game.msRunning
      |> makeTextBox (Text.height 50)
    message = makeTextBox (Text.height 50) <| 
      case game.state of
        GameOver -> "Game Over"
        Pause -> "Pause"
        _ -> ""
  in
    container w h middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
          |> filled bgBlack
        , group (append
          [ makeField colors
          , makePlayer game.player
          , group <| makeEnemies colors.bright game.enemies
          ]
          (makeCenterHole colors game)
        )
        |> rotate game.autoRotateAngle
        |> beatPulse game
      , toForm message 
        |> move (0, 40)
      , toForm score
          |> move (100 - halfWidth, halfHeight - 40)
      , toForm (
          if game.state == Play then spacer 1 1 else makeTextBox identity startMessage)
          |> move (0, 40 - halfHeight)
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

