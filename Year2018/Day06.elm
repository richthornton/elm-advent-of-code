module Year2018.Day06 exposing (..)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)

-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List Coordinate


type alias Input2 =
    List Coordinate


type alias Output1 =
    Int


type alias Output2 =
    Int

type alias CoordinateWithId =
    { x: Int
    , y: Int
    , id: Maybe CoordinateWithDistance
    }

type alias CoordinateWithDistance =
    { x: Int
    , y: Int,
    distance: Int
    }

type alias Coordinate =
    { x: Int
    , y: Int
    }

type alias Rect =
    {
        x1: Int,
        x2: Int,
        y1: Int,
        y2: Int
    }

-- 2. PARSE (mangle the input string into the representation we decided on)

parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.map createCoordinate

createCoordinate : String -> Coordinate
createCoordinate tupleString =
    let
        tupleAsArray = String.split ", " tupleString
        (x, y) =
            case tupleAsArray of
                [x1, y1] -> (Advent.unsafeToInt x1, Advent.unsafeToInt y1)
                _ -> Debug.todo "wrong input"
    in
        Coordinate x y

parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 selectedPoints =
    let
        maxValue = getDictionaryOfPoints selectedPoints
            |> Dict.values
            |> List.sort
            |> List.maximum
    in
        Maybe.withDefault -1 maxValue

getDictionaryOfPoints : Input1 -> Dict (Int, Int) Int
getDictionaryOfPoints selectedPoints =
    let
        boundingRect = selectedPoints
            |> findBoundingRect
        coordinatesWithClosestId = boundingRect
            |> findAllCoordinatesInRectangle
            |> List.map (findCoorindateWithLowestManhattenDistance selectedPoints)
        disallowedSelectedPoints = coordinatesWithClosestId
            |> findDisallowedSelectedPoints boundingRect
        allowedCoordinates = disallowedSelectedPoints
            |> filterOutdisallowedPoints coordinatesWithClosestId
        dict = allowedCoordinates
            |> groupCoorindateCounts
    in
        dict

debugRect : Rect -> Rect
debugRect rect =
    Debug.log (Debug.toString rect)
    rect

groupCoorindateCounts : List CoordinateWithId -> Dict (Int, Int) Int
groupCoorindateCounts coids =
    coids
        |> List.foldr
            (\coid carry ->
                case coid.id of
                    Just id -> 
                        Dict.update
                            (id.x, id.y)
                            (\existingCount ->
                                case existingCount of
                                    Just count ->
                                        Just (count + 1)

                                    Nothing ->
                                        Just 1
                            )
                            carry
                    Nothing -> carry
            )
            Dict.empty

filterOutdisallowedPoints : List CoordinateWithId -> List CoordinateWithDistance -> List CoordinateWithId
filterOutdisallowedPoints allPoints filterOuts =
    allPoints
        |> List.filter (notInList filterOuts)

notInList : List CoordinateWithDistance -> CoordinateWithId -> Bool
notInList disallowedCoos coo =
    let
        isInList = disallowedCoos
            |> List.any (hasSameCoordinates coo)
    in
        (not isInList)

hasSameCoordinates : CoordinateWithId -> CoordinateWithDistance -> Bool
hasSameCoordinates cwid cwd =
    case cwid.id of
        Just id -> id.x == cwd.x && id.y == cwid.y
        Nothing -> True

findDisallowedSelectedPoints : Rect -> List CoordinateWithId -> List CoordinateWithDistance
findDisallowedSelectedPoints rect coordinatesWithId =
    coordinatesWithId
        |> List.filter (isOnBoundingRect rect)
        |> List.map getIdFromCoordinateWithId

getIdFromCoordinateWithId : CoordinateWithId -> CoordinateWithDistance
getIdFromCoordinateWithId cwi =
    Maybe.withDefault { x = 1000000, y = 100000, distance = 1000000 } cwi.id

isOnBoundingRect : Rect -> CoordinateWithId -> Bool
isOnBoundingRect rect point =
    point.x == rect.x1 || point.x == rect.x2 || point.y == rect.y1 || point.y == rect.y2

findCoorindateWithLowestManhattenDistance : List Coordinate -> Coordinate -> CoordinateWithId
findCoorindateWithLowestManhattenDistance selectedPoints coordinate =
    let
        manhattenDistances = selectedPoints
            |> List.map (calculateManhattenDistance coordinate)
            |> List.sortBy .distance
    in
        case manhattenDistances of
            a :: b :: rest ->
                if isSameDistance a b then
                    {
                        x = coordinate.x,
                        y = coordinate.y,
                        id = Nothing
                    }
                else
                    {
                        x = coordinate.x,
                        y = coordinate.y,
                        id = Just a
                    }
    
            other ->
                {
                    x = coordinate.x,
                    y = coordinate.y,
                    id = Nothing
                }
    
isSameDistance : CoordinateWithDistance -> CoordinateWithDistance -> Bool
isSameDistance cwd1 cwd2 =
    cwd1.distance == cwd2.distance

calculateManhattenDistance : Coordinate -> Coordinate -> CoordinateWithDistance
calculateManhattenDistance coordinate1 coordinate2 =
    let
        absX = Basics.abs (coordinate1.x - coordinate2.x)
        absY = Basics.abs (coordinate1.y - coordinate2.y)
    in
    {
        x = coordinate2.x,
        y = coordinate2.y,
        distance = absX + absY
    }
    

findBoundingRect : List Coordinate -> Rect
findBoundingRect coordinates =
    let
        xValues = coordinates
            |> List.map extractX
        yValues = coordinates
            |> List.map extractY
        lowestX = Maybe.withDefault 1 (List.minimum xValues)
        biggestX = Maybe.withDefault 1 (List.maximum xValues)
        lowestY = Maybe.withDefault 1 (List.minimum yValues)
        biggestY = Maybe.withDefault 1 (List.maximum yValues)
    in
    {
        x1 = lowestX,
        x2 = biggestX,
        y1 = lowestY,
        y2 = biggestY
    }

debugIt : a -> a
debugIt a =
    Debug.log (Debug.toString a)
    a

findAllCoordinatesInRectangle : Rect -> List Coordinate
findAllCoordinatesInRectangle boundingRect = 
    let
        xValues = List.range boundingRect.x1 boundingRect.x2
        yValues = List.range boundingRect.y1 boundingRect.y2
    in
        xValues
            |> List.map (addYCoordinates yValues)
            |> List.concat
    
extractX : Coordinate -> Int
extractX coordinate =
    coordinate.x

extractY : Coordinate -> Int
extractY coordinate =
    coordinate.y

addYCoordinates : List Int -> Int -> List Coordinate
addYCoordinates yValues xValue =
    yValues
        |> List.map (addCoordinates xValue)

addCoordinates : Int -> Int -> Coordinate
addCoordinates x y =
    Coordinate x y

compute2 : Input2 -> Output2
compute2 selectedPoints =
    let
        boundingRect = selectedPoints
            |> findBoundingRect
        totalManhattendistances = boundingRect
            |> findAllCoordinatesInRectangle
            |> List.map (findTotalManhattenDistances selectedPoints)
            |> List.filter (isLessThan 10000)
    in
        List.length totalManhattendistances

isLessThan : Int -> Int -> Bool
isLessThan upperBound number =
    number < upperBound

findTotalManhattenDistances : List Coordinate -> Coordinate -> Int
findTotalManhattenDistances selectedPoints coordinate =
    let
        totalManhattenDistances = selectedPoints
            |> List.map (calculateManhattenDistance coordinate)
            |> List.map getDistance
            |> List.sum
    in
        totalManhattenDistances
        
getDistance : CoordinateWithDistance -> Int
getDistance coowd =
    coowd.distance

-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [Test "Test 1"
        """1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"""
        [
            Coordinate 1 1,
            Coordinate 1 6,
            Coordinate 8 3,
            Coordinate 3 4,
            Coordinate 5 5,
            Coordinate 8 9
        ]
        17
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
181, 184
230, 153
215, 179
84, 274
294, 274
127, 259
207, 296
76, 54
187, 53
318, 307
213, 101
111, 71
310, 295
40, 140
176, 265
98, 261
315, 234
106, 57
40, 188
132, 292
132, 312
97, 334
292, 293
124, 65
224, 322
257, 162
266, 261
116, 122
80, 319
271, 326
278, 231
191, 115
277, 184
329, 351
58, 155
193, 147
45, 68
310, 237
171, 132
234, 152
158, 189
212, 100
346, 225
257, 159
330, 112
204, 320
199, 348
207, 189
130, 289
264, 223
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
