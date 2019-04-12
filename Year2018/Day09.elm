module Year2018.Day09 exposing (..)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )

import Dict exposing (Dict)
import Array exposing (Array)

-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    GameEndState

type alias GameEndState =
    {
        numberOfPlayers: Int,
        lastMarbleScore: Int
    }

type alias Board = 
    {
        movesPlayed: MovesPlayed,
        currentPlayer: Int,
        players: Players,
        gameHasFinished: Bool
    }

type alias BoardDirection = String

type alias MovesPlayed = Array Int

type alias Players = Dict Int Player

type alias Player = 
    {
        movesToPlay: List Int,
        score: Int
    }

type alias Input2 =
    GameEndState


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    let
        numbersArray = string
            |> String.split ","
            |> List.map Advent.unsafeToInt
        (players, lastMarbleScore) =
            case numbersArray of
                [ l, r ] ->
                    ( l, r )

                _ ->
                    Debug.todo "wrong input 2"
    in
        {
            numberOfPlayers = players,
            lastMarbleScore = lastMarbleScore
        }
        


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)

debugIt : a -> a
debugIt a =
    Debug.log (Debug.toString a)
    a

compute1 : Input1 -> Output1
compute1 input =
    input
        |> initialiseGame
        |> doMoves
        |> getHighestScore

getHighestScore : Board -> Int
getHighestScore board =
    board
        |> .players
        |> Dict.values
        |> List.map .score
        |> List.maximum
        |> Maybe.withDefault 0

doMoves : Board -> Board
doMoves board =
    let
        newBoard = board
            |> doNextMove
    in
        case newBoard.gameHasFinished of
            True -> newBoard
            False -> doMoves newBoard

doNextMove : Board -> Board
doNextMove board =
    let
        player = board.currentPlayer
        nextMove = getNextMove board.players player
        (nextPlayer, hasFinished) = findNextPlayer player board.players
        (movesPlayed, newPlayersState) =
            case modBy 23 nextMove of
                0 ->
                    let
                        (mp, score1) = doSpecialMove board.movesPlayed
                        newScore = nextMove + score1
                        updatedPlayersState = addUpdatePlayer player board.players newScore
                    in
                        (mp, updatedPlayersState)
                _ ->
                    let
                        mp = doNormalMove board.movesPlayed nextMove
                        updatedPlayersState = addUpdatePlayer player board.players 0
                    in
                        (mp, updatedPlayersState)
    in
        {
            movesPlayed = movesPlayed,
            currentPlayer = nextPlayer,
            players = newPlayersState,
            gameHasFinished = hasFinished
        }

findNextPlayer : Int -> Players -> (Int, Bool)
findNextPlayer currentPlayer players =
    let
        possibleNextPlayer = currentPlayer + 1
        playersLength = List.length (Dict.keys players)
        nextPlayer =
            if possibleNextPlayer > playersLength then
                1
            else
                possibleNextPlayer
        hasFinished = hasGameFinised nextPlayer players
    in
        (nextPlayer, hasFinished)

hasGameFinised : Int -> Players -> Bool
hasGameFinised playerId players =
    let
        player = Dict.get playerId players
    in
        case player of
            Just playerUnwrapped ->
                let
                    p = playerUnwrapped
                in
                    List.isEmpty p.movesToPlay
            Nothing -> False
    
addUpdatePlayer : Int -> Players -> Int -> Players
addUpdatePlayer playerId players score =
    Dict.update
        (playerId)
        (\player ->
            case player of
                Just p -> 
                    let
                        updatedMoves =
                            case p.movesToPlay of
                                first :: rest -> rest
                                first -> []
                        newScore = p.score + score
                    in
                        Just {
                            movesToPlay = updatedMoves,
                            score = newScore
                        }
                Nothing ->
                    Nothing
        )
        players

getNextMove : Players -> Int -> Int
getNextMove players playerId =
    let
        player = Dict.get playerId players
    in
        case player of
            Just p ->
                let
                    move = List.head p.movesToPlay
                in
                    Maybe.withDefault 9999999 move
            Nothing -> 999999999

doNormalMove : MovesPlayed -> Int -> MovesPlayed
doNormalMove movesPlayed move =
    case movesPlayed of
        first :: second :: rest -> move :: rest ++ [first] ++ [second]
        first -> move :: first

doSpecialMove : MovesPlayed -> (MovesPlayed, Int)
doSpecialMove movesPlayed =
    let
        reversedList = List.reverse movesPlayed
        firstPartOfList = List.take 6 reversedList
        secondPartOfList = List.drop 6 reversedList
        (score, restOfList) =
            case secondPartOfList of
                s :: rest -> (s, rest)
                _ -> Debug.todo "Should not happen doSpecialMove"
        backToNormalFirstPart = List.reverse firstPartOfList
        backToNormalSecondPart = List.reverse restOfList
    in
        (backToNormalFirstPart ++ backToNormalSecondPart, score)

initialiseGame : GameEndState -> Board
initialiseGame gameEndState =
    let
        players = createPlayers gameEndState
    in
        {
            movesPlayed = Array.repeat 1 1,
            currentPlayer = 1,
            players = players,
            gameHasFinished = False
        }

createPlayers : GameEndState -> Players
createPlayers gameEndState =
    let
        allPlayerNumbers = List.range 1 gameEndState.numberOfPlayers
        allMoves = List.range 1 gameEndState.lastMarbleScore
        allPlayersInitialised = allPlayerNumbers
            |> List.map initialisePlayer
            |> Dict.fromList
        headOfList = Maybe.withDefault 1 (List.head allMoves)
        playersWithMoves = addMovesToPlayers headOfList (List.drop 1 allMoves) allPlayersInitialised
    in
        playersWithMoves

addMovesToPlayers : Int -> List Int -> Players -> Players
addMovesToPlayers move remainingMoves players =
    let
        playersWithMoves = addMoveToPlayers move players
    in
        case remainingMoves of
            first :: rest -> addMovesToPlayers first rest playersWithMoves
            [] -> playersWithMoves
    
    
addMoveToPlayers : Int -> Players -> Players
addMoveToPlayers move players =
    let
        playerToUpdate = players
            |> findPlayerToUpdate 
    in
        Dict.update
            (Tuple.first playerToUpdate)
            (\player ->
                case player of
                    Just p -> 
                        let
                            newMoves = List.append p.movesToPlay [move]
                        in
                            Just {
                                movesToPlay = newMoves,
                                score = 0
                            }
                    Nothing ->
                        Nothing
            )
            players

findPlayerToUpdate : Players -> (Int, Player)
findPlayerToUpdate players =
    let
        initialPlayer = Dict.get 1 players
        safeInitialPlayer = Maybe.withDefault (Tuple.second (initialisePlayer 1)) initialPlayer
    in
        players
            |> Dict.foldr (returnLowestPlayerId) (1, safeInitialPlayer)

returnLowestPlayerId : Int -> Player -> (Int, Player) -> (Int, Player)
returnLowestPlayerId lowestPayerId lowestPayer comparingPlaya =
    let
        comparingPlayaMoves = comparingPlaya
            |> Tuple.second
            |> .movesToPlay
        lowestPayerMoves = lowestPayer
            |> .movesToPlay
    in
        if List.length comparingPlayaMoves < List.length lowestPayerMoves then
            comparingPlaya
        else
            (lowestPayerId, lowestPayer)

initialisePlayer : Int -> (Int, Player)
initialisePlayer id =
    (id,
    {
        movesToPlay = [],
        score = 0
    }
    )

compute2 : Input2 -> Output2
compute2 input =
    compute1 {
        numberOfPlayers = input.numberOfPlayers,
        lastMarbleScore = input.lastMarbleScore * 100
    }


-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [Test "Test 1"
        "10,1618"
        {
            numberOfPlayers = 10,
            lastMarbleScore = 1618
        }
        8317,
    Test "Test 2"
        "13,7999"
        {
            numberOfPlayers = 13,
            lastMarbleScore = 7999
        }
        146373,
    Test "Test 3"
        "17,1104"
        {
            numberOfPlayers = 17,
            lastMarbleScore = 1104
        }
        2764,
    Test "Test 4"
        "21,6111"
        {
            numberOfPlayers = 21,
            lastMarbleScore = 6111
        }
        54718,
    Test "Test 5"
        "30,5807"
        {
            numberOfPlayers = 30,
            lastMarbleScore = 5807
        }
        37305,
    Test "Test example"
        "9,25"
        {
            numberOfPlayers = 9,
            lastMarbleScore = 25
        }
        32
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
459,71320
"""
        |> Advent.removeNewlinesAtEnds


main : Program () ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input_
        , parse1 = parse1
        , parse2 = parse2
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }
