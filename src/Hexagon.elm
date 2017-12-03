module Main exposing (..)

import Time exposing (..)
import List exposing (..)
import Tuple exposing (..)
import AnimationFrame
import Keyboard.Extra exposing (Key(..))
import Window
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Html exposing (Html)
import Debug
import Text
import Audio exposing (PlaybackOptions, defaultPlaybackOptions, Sound)
import Task exposing (Task, andThen)
import String exposing (padLeft)


-- MODEL


type State
    = Loading
    | NewGame
    | Starting
    | Play
    | Pausing
    | Pause
    | Resume
    | GameOver


type alias Player =
    { angle : Float }


type Direction
    = Left
    | Right
    | Still


type alias Enemy =
    { radius : Float
    , parts : List (Bool)
    }


type alias Game =
    { player : Player
    , direction : Direction
    , enemies : List (Enemy)
    , enemySpeed : Float
    , pressedKeys : List Key
    , state : State
    , timeStart : Time
    , timeTick : Time
    , msRunning : Float
    , autoRotateAngle : Float
    , autoRotateSpeed : Float
    , hasBass : Bool
    , music : Maybe Sound
    }


type alias Colors =
    { dark : Color
    , medium : Color
    , bright : Color
    }


type Msg
    = Step Time
    | KeyboardMsg Keyboard.Extra.Msg
    | MusicLoaded Sound
    | Error String
    | Noop


( gameWidth, gameHeight ) =
    ( 1024, 576 )
( halfWidth, halfHeight ) =
    ( gameWidth / 2, gameHeight / 2 )
( iHalfWidth, iHalfHeight ) =
    ( gameWidth // 2, gameHeight // 2 )
playerRadius : Float
playerRadius =
    gameWidth / 10.0


playerSize : Float
playerSize =
    10.0


playerSpeed : Float
playerSpeed =
    0.12


enemyThickness =
    30


enemyDistance =
    350


enemyInitialSpeed =
    0.25


enemyAcceleration =
    0.000002


enemies =
    [ [ False, True, False, True, False, True ]
    , [ False, True, True, True, True, True ]
    , [ True, False, True, True, True, True ]
    , [ True, True, True, True, False, True ]
    , [ True, True, True, False, True, True ]
    , [ False, True, False, True, False, True ]
    , [ True, False, True, False, True, False ]
    , [ True, True, True, True, True, False ]
    ]


startMessage =
    "SPACE to start, &larr;&rarr; to move"


beat =
    138.0 |> bpm


beatAmplitude =
    0.06


beatPhase =
    270 |> degrees



-- Calculate Beat Per Minute


bpm : Float -> Float
bpm beat =
    (2.0 * pi * beat / 60)


pump : Float -> Float
pump progress =
    beatAmplitude * (beat * progress / 1000 + beatPhase |> sin)



-- MUSIC


hasBass : Time -> Bool
hasBass time =
    if time < 20894 then
        False
    else if time < 41976 then
        True
    else if time < 55672 then
        False
    else if time < 67842 then
        True
    else if time < 187846 then
        False
    else if time < 215938 then
        True
    else
        False


loadSound : Task String Sound
loadSound =
    Audio.loadSound "music/shinytech.mp3"


soundLoaded : Result String Sound -> Msg
soundLoaded result =
    case result of
        Ok music ->
            MusicLoaded music

        Err msg ->
            Error msg


playbackOptions =
    { defaultPlaybackOptions
        | loop = True
        , startAt = Nothing
    }


playSound : Sound -> PlaybackOptions -> Cmd Msg
playSound sound options =
    Task.attempt (always Noop) (Audio.playSound options sound)


stopSound : Sound -> Cmd Msg
stopSound sound =
    Task.perform (always Noop) (Audio.stopSound sound)


bgBlack : Color
bgBlack =
    rgb 20 20 20



-- UPDATE


updatePlayerAngle : Float -> Direction -> Float
updatePlayerAngle angle dir =
    let
        sign =
            if dir == Left then
                1
            else if dir == Right then
                -1
            else
                0

        newAngle =
            angle + toFloat sign * playerSpeed
    in
        if newAngle < 0 then
            newAngle + 2 * pi
        else if newAngle > 2 * pi then
            newAngle - 2 * pi
        else
            newAngle


colidesWith : Player -> Enemy -> Bool
colidesWith player enemy =
    let
        collidesAtIndex : Int -> Bool
        collidesAtIndex index =
            let
                fromAngle =
                    (toFloat index) * 60

                toAngle =
                    ((toFloat index) + 1) * 60

                playerDegrees =
                    player.angle * 360 / (2 * pi)
            in
                playerDegrees >= fromAngle && playerDegrees < toAngle
    in
        if enemy.radius > playerRadius || enemy.radius + enemyThickness < playerRadius - playerSize * 3 / 2 then
            False
        else
            -- check if open
            indexedMap (,) enemy.parts |> filter Tuple.second |> map Tuple.first |> any collidesAtIndex


updatePlayer : Direction -> Game -> Player
updatePlayer dir { player, enemies, state } =
    if state == Play then
        let
            newAngle =
                if state == NewGame then
                    degrees 30
                else
                    updatePlayerAngle player.angle dir

            newPlayer =
                { player | angle = newAngle }
        in
            -- stop rotating if there is an enemy passing the ship
            if any (colidesWith newPlayer) enemies then
                player
            else
                newPlayer
    else
        player


isGameOver : Game -> Bool
isGameOver { player, enemies } =
    any (colidesWith player) enemies


updateMsRunning : Time -> Game -> Time
updateMsRunning timestamp game =
    case game.state of
        Play ->
            game.msRunning + timestamp - game.timeTick

        NewGame ->
            0.0

        _ ->
            game.msRunning


updateAutoRotateAngle : Game -> Float
updateAutoRotateAngle { autoRotateAngle, autoRotateSpeed } =
    autoRotateAngle + autoRotateSpeed


updateAutoRotateSpeed : Game -> Float
updateAutoRotateSpeed { msRunning, autoRotateSpeed } =
    0.02 * sin (msRunning * 0.0003)


updateEnemies : Game -> List (Enemy)
updateEnemies game =
    let
        enemyProgress =
            game.msRunning * game.enemySpeed

        numEnemies =
            List.length enemies

        maxDistance =
            numEnemies * enemyDistance

        offsetForEnemy index =
            round <| enemyDistance * (toFloat index) - enemyProgress

        radiusFor index =
            (offsetForEnemy index)
                % maxDistance
                |> toFloat
    in
        List.indexedMap
            (\index parts ->
                { parts = parts
                , radius = radiusFor index
                }
            )
            enemies


updateEnemySpeed : Game -> Float
updateEnemySpeed game =
    enemyInitialSpeed + game.msRunning * enemyAcceleration


{-| Updates the game state on a keyboard command
-}
onUserInput : Keyboard.Extra.Msg -> Game -> ( Game, Cmd Msg )
onUserInput keyMsg game =
    let
        pressedKeys =
            Keyboard.Extra.update keyMsg game.pressedKeys

        spacebar =
            List.member Keyboard.Extra.Space pressedKeys
                && not (List.member Keyboard.Extra.Space game.pressedKeys)

        direction =
            if (Keyboard.Extra.arrows pressedKeys).x < 0 then
                Left
            else if (Keyboard.Extra.arrows pressedKeys).x > 0 then
                Right
            else
                Still

        nextState =
            case game.state of
                NewGame ->
                    if spacebar then
                        Starting
                    else
                        NewGame

                Play ->
                    if spacebar then
                        Pausing
                    else
                        Play

                GameOver ->
                    if spacebar then
                        NewGame
                    else
                        GameOver

                Pause ->
                    if spacebar then
                        Resume
                    else
                        Pause

                _ ->
                    game.state
    in
        ( { game
            | pressedKeys = pressedKeys
            , direction = direction
            , state = nextState
          }
        , Cmd.none
        )


{-| Updates the game state on every frame
-}
onFrame : Time -> Game -> ( Game, Cmd Msg )
onFrame time game =
    let
        ( nextState, nextCmd ) =
            case game.music of
                Nothing ->
                    ( Loading, Cmd.none )

                Just music ->
                    case game.state of
                        Starting ->
                            ( Play, playSound music { playbackOptions | startAt = Just 0 } )

                        Resume ->
                            ( Play, playSound music playbackOptions )

                        Pausing ->
                            ( Pause, stopSound music )

                        Play ->
                            if isGameOver game then
                                ( GameOver, stopSound music )
                            else
                                ( Play, Cmd.none )

                        _ ->
                            ( game.state, Cmd.none )
    in
        ( { game
            | player = updatePlayer game.direction game
            , enemies = updateEnemies game
            , enemySpeed = updateEnemySpeed game
            , state = nextState
            , timeStart =
                if game.state == NewGame then
                    time
                else
                    game.timeStart
            , timeTick = time
            , msRunning = updateMsRunning time game
            , autoRotateAngle = updateAutoRotateAngle game
            , autoRotateSpeed = updateAutoRotateSpeed game
            , hasBass = hasBass game.msRunning
          }
        , nextCmd
        )


{-| Game loop: Transition from one state to the next.
-}
update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        KeyboardMsg keyMsg ->
            onUserInput keyMsg game

        Step time ->
            onFrame time game

        MusicLoaded music ->
            ( { game
                | state = NewGame
                , music = Just music
              }
            , Cmd.none
            )

        Error message ->
            Debug.crash message

        _ ->
            ( game, Cmd.none )



-- VIEW


moveRadial : Float -> Float -> Form -> Form
moveRadial angle radius =
    move ( radius * cos angle, radius * sin angle )


makePlayer : Player -> Form
makePlayer player =
    let
        angle =
            player.angle - degrees 30
    in
        ngon 3 playerSize
            |> filled (hsl angle 1 0.5)
            |> moveRadial angle (playerRadius - playerSize)
            |> rotate angle


trapezoid : Float -> Float -> Color -> Form
trapezoid base height color =
    let
        s =
            height / (tan <| degrees 60)
    in
        filled color
            <| polygon
                [ ( -base / 2, 0 )
                , ( base / 2, 0 )
                , ( base / 2 - s, height )
                , ( -base / 2 + s, height )
                ]


makeEnemy : Color -> Enemy -> Form
makeEnemy color enemy =
    let
        base =
            2.0 * (enemy.radius + enemyThickness) / (sqrt 3)

        makeEnemyPart : Int -> Form
        makeEnemyPart index =
            trapezoid base enemyThickness color
                |> rotate (degrees <| toFloat (90 + index * 60))
                |> moveRadial (degrees <| toFloat (index * 60)) (enemy.radius + enemyThickness)
    in
        group (indexedMap (,) enemy.parts |> filter Tuple.second |> map Tuple.first |> map makeEnemyPart)


makeEnemies : Color -> List (Enemy) -> List (Form)
makeEnemies color enemies =
    map (makeEnemy color) enemies


hexagonElement : Int -> List ( Float, Float )
hexagonElement i =
    let
        radius =
            halfWidth * sqrt 2

        angle0 =
            60 * i |> toFloat |> degrees

        angle1 =
            60 * (i + 1) |> toFloat |> degrees
    in
        [ ( 0.0, 0.0 )
        , ( sin angle0 * radius, cos angle0 * radius )
        , ( sin angle1 * radius, cos angle1 * radius )
        ]


makeField : Colors -> Form
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
        group (map poly (List.range 0 5))



-- the polygon in the center: this is just decoration, so it has no own state


makeCenterHole : Colors -> Game -> List Form
makeCenterHole colors game =
    let
        bassAdd =
            if game.hasBass then
                0
            else
                100.0 * (pump game.msRunning)

        shape =
            ngon 6 (60 + bassAdd)

        line =
            solid colors.bright
    in
        [ shape
            |> filled colors.dark
            |> rotate (degrees 90)
        , shape
            |> (outlined { line | width = 4.0 })
            |> rotate (degrees 90)
        ]


makeColors : Float -> Colors
makeColors msRunning =
    let
        hue =
            0.00005 * msRunning
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
        scale (1 + (pump game.msRunning))
    else
        identity


formatTime : Time -> String
formatTime running =
    let
        centiseconds =
            floor (Time.inMilliseconds running / 10)

        seconds =
            centiseconds // 100

        centis =
            centiseconds % 100
    in
        padLeft 3 '0' (toString seconds) ++ "." ++ padLeft 2 '0' (toString centis)


view : Game -> Html.Html Msg
view game =
    let
        bg =
            rect gameWidth gameHeight |> filled bgBlack

        colors =
            makeColors game.msRunning

        score =
            formatTime game.msRunning |> makeTextBox 50

        message =
            makeTextBox 50
                <| case game.state of
                    Loading ->
                        "Loading..."

                    GameOver ->
                        "Game Over"

                    Pause ->
                        "Pause"

                    _ ->
                        ""

        field =
            append
                [ makeField colors
                , makePlayer game.player
                , group <| makeEnemies colors.bright game.enemies
                ]
                (makeCenterHole colors game)
                |> group
    in
        toHtml
            <| container gameWidth gameHeight middle
            <| collage gameWidth
                gameHeight
                [ bg
                , field |> rotate game.autoRotateAngle |> beatPulse game
                , toForm message |> move ( 0, 40 )
                , toForm score |> move ( 100 - halfWidth, halfHeight - 40 )
                , toForm
                    (if game.state == Play then
                        spacer 1 1
                     else
                        makeTextBox 20 startMessage
                    )
                    |> move ( 0, 40 - halfHeight )
                ]



-- SUBSCRIPTIONS


subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch
        [ AnimationFrame.times (\time -> Step time)
        , Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        ]



--INIT


init : ( Game, Cmd Msg )
init =
    ( { player = Player (degrees 30)
      , pressedKeys = []
      , direction = Still
      , state = NewGame
      , enemies = []
      , enemySpeed = 0.0
      , timeStart = 0.0
      , timeTick = 0.0
      , msRunning = 0.0
      , autoRotateAngle = 0.0
      , autoRotateSpeed = 0.0
      , hasBass = False
      , music = Nothing
      }
    , Task.attempt soundLoaded loadSound
    )


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
