module Year2018.Day02 exposing (..)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)

-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List String


type alias Input2 =
    List String


type alias Output1 =
    Int


type alias Output2 =
    String



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    String.lines string


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    let 
        groupedChars = input
            |> List.map String.toList
            |> List.map groupChars
        twoOccurances = computeTwoOccurances groupedChars
        threeOccurances = computeThreeOccurances groupedChars
    in
        twoOccurances * threeOccurances

compute2 : Input2 -> Output2
compute2 input =
    let
        allWordsAsChars = input
            |> List.map String.toList
        lowestChars = allWordsAsChars
            |> List.map (findLowest allWordsAsChars)
        lowest = lowestChars
            |> List.sortBy Tuple.second
            |> List.head
            |> Maybe.withDefault ("This is not the correct answer", 100000)
    in
        Tuple.first lowest

findLowest : List (List Char) -> List Char -> (String, Int)
findLowest allWords word = 
    let
        lowestItem = allWords
            |> List.map (findMatches word)
            |> List.sortBy Tuple.second
            |> List.filter isNonZeroTuple
            |> List.head
            |> Maybe.withDefault ("asdfasdfsadf", 1000000)
    in
        lowestItem

isNonZeroTuple : (String, Int) -> Bool
isNonZeroTuple tuple =
    (Tuple.second tuple) /= 0

findMatches : List Char -> List Char -> (String, Int)
findMatches list1 list2 = 
    let
        lengthDifference = abs (List.length list1 - List.length list2)
        charTuples = List.map2 Tuple.pair list1 list2
        matchingLetters = charTuples 
            |> List.filter sameChar
            |> List.map Tuple.first
            |> String.fromList
        nonMatchingNumber = charTuples
            |> List.filter notSameChar
            |> List.length
    in
        (matchingLetters, nonMatchingNumber + lengthDifference)

notSameChar : (Char, Char) -> Bool
notSameChar chars =
    not (sameChar chars)

sameChar : (Char, Char) -> Bool
sameChar chars = 
    Tuple.first chars == Tuple.second chars

groupChars : List Char -> Dict Char Int
groupChars chars =
    chars
        |> List.foldr
            (\char carry ->
                Dict.update
                    char
                    (\existingCount ->
                        case existingCount of
                            Just count ->
                                Just (count + 1)

                            Nothing ->
                                Just 1
                    )
                    carry
            )
            Dict.empty

computeTwoOccurances : List (Dict Char Int) -> Int
computeTwoOccurances all =
    all
        |> List.map Dict.values
        |> List.filter containsNumber2
        |> List.length

computeThreeOccurances : List (Dict Char Int) -> Int
computeThreeOccurances all =
    all
        |> List.map Dict.values
        |> List.filter containsNumber3
        |> List.length

containsNumber2 : List Int -> Bool
containsNumber2 values = 
    containsNumber 2 values

containsNumber3 : List Int -> Bool
containsNumber3 values = 
    containsNumber 3 values

containsNumber : Int -> List Int -> Bool
containsNumber number values =
    values
        |> List.any (\val -> val == number)

-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [Test "Test 1"
        "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab"
        ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
        12
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [Test "Test 1"
        "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz"
        ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
        "fgij"
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
pbopcmjeizuhxlqnwasfgtycdm
pjokrmjeizuhxlqnfasfguycdv
pbokrpjejkuhxlqnwasfgtycdv
sbokrmjeizuhxaqnwastgtycdv
pbokrmjeizuhxljnnasfgtycnv
pbokrqjeizuhxbqndasfgtycdv
bbokrmjeizuhxlqnwasfgtycfj
pbokrmjeisuhxlqnwasfzdycdv
pbokrmjefxuhxlqnwasfptycdv
pqokrmjenzuhxlqnwasfgtygdv
pbokrmjeizunklqnwassgtycdv
pbokrmjeizghxvqnkasfgtycdv
lboirmjeizuhxlqnwfsfgtycdv
pbofrmjeizuhalqnwasfgtyddv
pbokrmjeiguhplqcwasfgtycdv
pbokrmjeizzhxlqnwavfgtyctv
pbokrmjeizuhxlqnwaefgtycaj
pbokzmjedzuhxlqnwasfgtlcdv
pnokrmjegzuhxlbnwasfgtycdv
pbojrmjeizuhtlqniasfgtycdv
pbokxmiefzuhxlqnwasfgtycdv
pbokrmoeizuhxlqnpasngtycdv
abokrmjeezuhxlqnwasfdtycdv
pbokrmyeizugxlqnwasfgtycda
pbokdmjeizuhxlqnuatfgtycdv
psokrmjeiauhxlqnwasxgtycdv
pbokrmjeizuhxlqzwasfgtyzdy
pboktmjeizuhxnqndasfgtycdv
pbodrrjeizuhxlqnwasfgtycdb
pbokrmjekzuhxljnwasfgtycuv
pbokrmjnizuhllqnwawfgtycdv
prmkrmjeiwuhxlqnwasfgtycdv
pbokrmjeizkhxlenwajfgtycdv
pbofrmjeizuixlqnwasfgoycdv
gbhkrmjeizuhclqnwasfgtycdv
pbokrmweizuwxlqnwasfgtycyv
pbukrmjeizuhxlqnwasfgqhcdv
pbokxmjeizuhxlqnwasfgtecdu
pbokomjeizuhrlqnfasfgtycdv
bbokymjeizuhxlqnpasfgtycdv
pbodrmjeizuhxlqnwadfgtgcdv
zbokrljeipuhxlqnwasfgtycdv
pboermjeizuxxlqnwasfgtpcdv
pqbkrmjeizuhxlqnjasfgtycdv
pbokrmfeizuhxvqgwasfgtycdv
pbokrmjeizuhzlqnjasfdtycdv
rbofrmjeizkhxlqnwasfgtycdv
pbokrmseizubxlqnwasfgtycdy
pbocrmjeizuhxaqnwasfgtycda
pbokrmjeizuhxlqndakfgtscdv
pbokrrjeizuhxlqnwgshgtycdv
pbokrajeizuhxpqnwasrgtycdv
pbokrbjeizubxlqnwssfgtycdv
pbokemjhizuhxlqnwazfgtycdv
pbokrmjeizuhxlqntisfgtyrdv
pbokrmjwinuhxlqnwasfgkycdv
pypkrmjeizuhxlqtwasfgtycdv
pbokrmjeizuhxlqniasfrpycdv
pbokomjeizuhxlqnwasfgtgcdw
pbokrmjeizusplqnwxsfgtycdv
pbodrmueizxhxlqnwasfgtycdv
pbokwmjeizurxlqnwasfgtycdi
pbohrmjejzuhxlqnwasfgtycgv
pbokrmtqizuhxlqnwasfitycdv
ptozrmjeizuhylqnwasfgtycdv
pbokrmjtizuhxlqfwasfgtykdv
pbokrmpeizuhxnqmwasfgtycdv
pbokrmjeizujxlynwtsfgtycdv
dbokrmjeizuhxlqnwasngticdv
pbskrmjeizuhxlqnrasfttycdv
pbwkrmjerzuhxlqnwaslgtycdv
pboyrmceizuhxlqnwssfgtycdv
pbokpmjeizchxlqngasfgtycdv
pbokrmjenzuhxlqnwcsfgxycdv
pbxkrmjeizuhxlqnwadfgtyckv
pbqkrmjeizuhxlqnwasdgdycdv
pbokrmoeizdhxlqnwasfgtycqv
pbokrmjejzuhxlqnwksfgtycwv
pbfkrmjeieuhxlnnwasfgtycdv
pbokrmjeiuuhxlqnpalfgtycdv
pbokrmjeizunxyqnwasfgtdcdv
pbokrmjeazuhxrqnwasogtycdv
pbmkrmjeizuhxlqnwaufgtycdj
xbskrmjeipuhxlqnwasfgtycdv
tbokrujlizuhxlqnwasfgtycdv
xbokvmjeizuhxyqnwasfgtycdv
pbnhrmheizuhxlqnwasfgtycdv
pboorajrizuhxlqnwasfgtycdv
pbokrmjeizuhxminwusfgtycdv
pboqrmjeizuhxlqnwaslgtscdv
pgokrdjeizuhxlnnwasfgtycdv
pbokrmjeizuhxiqnwasvttycdv
pbokrmwnizuhzlqnwasfgtycdv
pbokrmjlizmhjlqnwasfgtycdv
pbwkrmjeizohxlqnwasfgtyzdv
pbykrmjmizwhxlqnmasfgtycdv
pbokrmjzizuhxeqnwasfgtpcdv
pbokrmjewzuhxzqnwasfgtybdv
pbokrmjeimupxlonwasfgtycdv
pbokrmjvizuhxlqnuasfgtycqv
pbokrmjeizjdxlqnwasfetycdv
pbofrmjeizurxlqnwasfztycdv
pbozrmjeizuhxxqpwasfgtycdv
pbovtmjeizuhxlqnwapfgtycdv
prokrmjeuzuhxlqnwasfgtycqv
ubokrmjeizuhxljnwasfgtdcdv
pboknmjepzuhxlqnwasogtycdv
pbokrmjaizuaxljnwasfgtycdv
pbdkrcjeizuhxlqnwasfgtvcdv
pbokymjeizuhxlqnwaxfgtyfdv
pbokrmjaizuhxlqnfasfgtyodv
pdekrmmeizuhxlqnwasfgtycdv
rbokrmjeizuuxlqnwasfgtycdj
pbokrmneifuhxlqiwasfgtycdv
pbokrmjeizlbxlunwasfgtycdv
pbokrmjewzuhxxqnwasfgoycdv
pbokrmjeizuhxlqtwasfgtzcdo
pbokrmkeizzhxlqnwasfgtycmv
pbokrmjeiquhxlqnywsfgtycdv
xbokrmjeizjhxlqdwasfgtycdv
pbokrmjeizahxzqnzasfgtycdv
pbokrmjeizuhxmqmwasfgtytdv
pbokrmheiluhxlqnwasfgoycdv
rbokrmjeizuhxlqnwaslgtycqv
pbbkzmjeizuhxvqnwasfgtycdv
pbokrmjeizudxlznwgsfgtycdv
pbokemjeizuhxlqnwascgtysdv
pbokrmjdizuexlgnwasfgtycdv
pbokzmjeizuhxlqnwnsfggycdv
pbokrmjeizuhxtqnwasfgiycdy
bbokrmjeizuhclunwasfgtycdv
pbtkrmjeieuhxlqnwasfgtycrv
pbokrmjeizutxlbnwasngtycdv
pbokrmjevzumxlqnwasfgtyydv
pbokrmjsizuhxlqowasfgtycyv
pbssrmjeizuhxlqbwasfgtycdv
pbokrmjeizuhflqnwxsfstycdv
pbokimjeizuhxlqnwasfgtywdm
pbokrmjbizuhxlqdwasfgtygdv
pbokrmheizuhxlqxwasfgtycnv
poakrmjeizuhylqnwasfgtycdv
vbrkrmjeizuhxlqnwaszgtycdv
pbokrmjeizuhxiqnudsfgtycdv
pbokrldeizuhxlqnwasjgtycdv
pbokrmjeizjhflqnwasfgtymdv
pbokrmjeizuhxliawasfgtvcdv
pbokrmjeisuhtoqnwasfgtycdv
nbokrijeizuhxlqnwasfgtycdh
pbokrmjeizrhxlqnwxsfztycdv
pbotrmjeizuhxlcnwasfgtyvdv
pbokrmjewzuhxlquwasfgtjcdv
pbosrmjeipuhxlqnwasfgtvcdv
pbokrmjebzurxlunwasfgtycdv
pbogimieizuhxlqnwasfgtycdv
pbokrmjeizihxlqnwasagtyzdv
pbokrmjeizuoxlqnausfgtycdv
pbokrmjeizuhxlqnwashgbjcdv
pbokrdjeizuhxlnnwasfgoycdv
pbokrzjtizlhxlqnwasfgtycdv
peokrmjexzuhxlqnwasfgoycdv
cboprmjeizuhxlqnwasfgfycdv
pbitrmjeizjhxlqnwasfgtycdv
pbourmjeizuhxldnwjsfgtycdv
pboivmjeizuhxlqnwasvgtycdv
pbokrmjeiduhxaqnqasfgtycdv
pbokicjeiwuhxlqnwasfgtycdv
pbokrmmeizulxlqnwasfgtyvdv
pbokrmjeieuhxlqnaapfgtycdv
pbokxmjeiuuhxlqnwasfgtyqdv
pbokrmjeizuhxgqniaslgtycdv
pbokrmjeizuuxlqnwisfgtyckv
pbovlmjepzuhxlqnwasfgtycdv
pbokrmjeizuhxlqdwaqfgtycdj
pbztrvjeizuhxlqnwasfgtycdv
pbokrmjeizuholunwasfptycdv
pbokrmjeizudxlqnwusfgtycqv
nbokrmjzizmhxlqnwasfgtycdv
pbokrmjeypunxlqnwasfgtycdv
pbokrjjxizuhxlqnwasfgtyddv
pbokrmjeizuhilqnwiufgtycdv
pbokrmjeizuhxtqowasfgfycdv
qbokrgjeizuhxlqnwasfgtycdx
pvoarmjeizuhxlqnwasfgtfcdv
pbokrmjjizuhxlqnwasfggyczv
pbtkrmjeizuhnlqncasfgtycdv
pbokrmjeizuzxlqnwasfgtyjnv
jmokrmzeizuhxlqnwasfgtycdv
pbykrmjmizwhxlqnwasfgtycdv
nbokrmjeizlhxlqnwasfgtecdv
pbokrmjeizuhxlqhwasrgrycdv
pbokrmjeiruhxlqnwasfgtnedv
pbokrmjeizohxlznwasfgtycuv
paokrmjdizuhxlqnwasfktycdv
pbokrmjetzutxlqnwasfntycdv
pboyrmjeizuhxlqnwasfgtetdv
pbokgujeizuhxlqwwasfgtycdv
pbokrifeizshxlqnwasfgtycdv
sbokrmjeizfhxlqnaasfgtycdv
pbokrmjeizuhxlqpwrsfgfycdv
pbokxmjeikuhxlqnwasfctycdv
fbokrmjhizuhxlqnmasfgtycdv
pbekamjeizuhxlqnwaxfgtycdv
pboksmpeizuhxlqnwasfgtyclv
pbokrmjeizrhxdqnwasfgzycdv
pbogrmxeizurxlqnwasfgtycdv
pbokrmjeieuhxlqnwqsfgtychv
vbokrmjeizuhxlqnwabfgtycdq
lbokrmjeizupxlqvwasfgtycdv
pbokrmjeizuhglqnuasfgtucdv
hbokrmjeizuhelqnwasfgtrcdv
pbokrmweizuhxlqnwhsfgtyvdv
pbokrmjeizuhxrqnwasfvtccdv
pbokrmneizuhxlwnyasfgtycdv
ybokymjeqzuhxlqnwasfgtycdv
pbousmjeizuhxlqswasfgtycdv
pblkimjeizuhxlqnwacfgtycdv
psokrmjeizuhxlqnwasfgbpcdv
peokrwjeizghxlqnwasfgtycdv
pbokrmjeizudxlqnwzsfrtycdv
pbotrmjezzxhxlqnwasfgtycdv
pkokrmjezzuhxlqnwasfgtycdh
pbokrmleizuhxlnnwasfgtyndv
pboxwmjeituhxlqnwasfgtycdv
pbokrmjeizoczlqnwasfgtycdv
pbokomjeizuhxlqnwhsfgtybdv
pbhwrmjeizuhxlqnwasfgpycdv
pbwkrmjeizuhxeqnwasfgtyidv
pbokrmjeizuhxlqnjasfgmicdv
tbokrgjeizuhxlqhwasfgtycdv
pbolrqjeizuhxlqnhasfgtycdv
pbogrhjeizbhxlqnwasfgtycdv
pbokrmjeizghxlqnwashgtycdx
pbokrmjeizuhrlqnwasfgthcrv
pbokrmjeizuhxlqnwfsngtacdv
pbokrmxeizuhxlqnwasfotyctv
pbokrmjeizuhxlqnwcsfgnocdv
pnokbmjeizuhxlqnwasfgtscdv
pbowrmjeuzuhxlqnwasfgtycdw
pbokrmjeiyuhxlqnwasqgtvcdv
pbokrmjeivuhxkpnwasfgtycdv
pbokomjeizuhxlqnwasfgtylav
pbdkrmjeizuhxlgnwjsfgtycdv
pbokrmjeizuaxxqnwasfytycdv
pbokrmjerzuhxlqnwasfgtscdk
pbokrmzerzuhxlqnwasfntycdv
pbokrmjeizumxdqnwasfgtyckv
pbtkrmjeizrhxlqnwasfgtjcdv
pbmkrmjuizuhxlqnwasfgtytdv
pbokpmjeizuhxlqnwastgtzcdv
kbokrmjeizuhxlqnwasfgzjcdv
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
