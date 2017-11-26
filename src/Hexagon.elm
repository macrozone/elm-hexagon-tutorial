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


view : Game -> Html.Html Msg
view game =
    let
        bg =
            rect gameWidth gameHeight |> filled bgBlack

        field =
            makePlayer game.player
    in
        toHtml
            <| container gameWidth gameHeight middle
            <| collage gameWidth
                gameHeight
                [ bg
                , field
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
