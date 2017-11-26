module Main exposing (..)

import Time exposing (..)
import List exposing (..)
import AnimationFrame
import Keyboard.Extra exposing (Key(..))
import Window
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Html exposing (Html)
import Debug


-- MODEL


type State
    = NewGame
    | Play
    | GameOver
    | Pause



-- MODEL


type alias Player =
    { angle : Float }


type Direction
    = Left
    | Right
    | Still


type alias Game =
    { player : Player
    , direction : Direction
    , pressedKeys : List Key
    , state : State
    , timeStart : Time
    , timeTick : Time
    , msRunning : Float
    , autoRotateAngle : Float
    , autoRotateSpeed : Float
    }


type alias Colors =
    { dark : Color
    , medium : Color
    , bright : Color
    }


type Msg
    = Step Time
    | KeyboardMsg Keyboard.Extra.Msg


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


updatePlayer : Direction -> Game -> Player
updatePlayer dir { player, state } =
    if state == Play then
        let
            newAngle =
                if state == NewGame then
                    degrees 30
                else
                    updatePlayerAngle player.angle dir
        in
            { player | angle = newAngle }
    else
        player


isGameOver : Game -> Bool
isGameOver { player } =
    False


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
    0.02
        * sin (msRunning * 0.0003 |> Debug.log "φ")
        |> Debug.log "autoRotateSpeed"


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
                        Play
                    else
                        NewGame

                Play ->
                    if spacebar then
                        Pause
                    else
                        Play

                GameOver ->
                    if spacebar then
                        NewGame
                    else
                        GameOver

                Pause ->
                    if spacebar then
                        Play
                    else
                        Pause
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
        nextCmd =
            Cmd.none

        nextState =
            case game.state of
                NewGame ->
                    NewGame

                Play ->
                    if isGameOver game then
                        GameOver
                    else
                        Play

                _ ->
                    game.state
    in
        ( { game
            | player = updatePlayer game.direction game
            , state = Debug.log "state" nextState
            , timeStart =
                if game.state == NewGame then
                    time
                else
                    game.timeStart
            , timeTick = time
            , msRunning = Debug.log "msRunning" (updateMsRunning time game)
            , autoRotateAngle = updateAutoRotateAngle game
            , autoRotateSpeed = updateAutoRotateSpeed game
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
        shape =
            ngon 6 60

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


view : Game -> Html.Html Msg
view game =
    let
        bg =
            rect gameWidth gameHeight |> filled bgBlack

        colors =
            makeColors game.msRunning

        field =
            append
                [ makeField colors
                , makePlayer game.player
                ]
                (makeCenterHole colors game)
                |> group
    in
        toHtml
            <| container gameWidth gameHeight middle
            <| collage gameWidth
                gameHeight
                [ bg
                , field |> rotate game.autoRotateAngle
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
      , timeStart = 0.0
      , timeTick = 0.0
      , msRunning = 0.0
      , autoRotateAngle = 0.0
      , autoRotateSpeed = 0.0
      }
    , Cmd.none
    )


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
