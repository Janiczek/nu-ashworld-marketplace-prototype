module Tests exposing (suite)

import ArchitectureTest as ATest exposing (TestedApp)
import Backend
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, map, oneOf, string)
import Lamdera exposing (ClientId)
import List.Extra
import MaxPriorityQueue
import MinPriorityQueue
import SeqDict
import Set
import Test exposing (Test)
import Types exposing (..)


type Msg
    = BackendMsg BackendMsg
    | ToBackendMsg Lamdera.ClientId ToBackend


model_ : BackendModel
model_ =
    Backend.initModel
        |> guaranteePlayers


guaranteePlayers : BackendModel -> BackendModel
guaranteePlayers model =
    let
        wantedPlayers =
            List.range 1 5
                |> List.map String.fromInt

        neededPlayers =
            model.players
                |> Dict.keys
                |> Set.fromList
                |> Set.diff (Set.fromList wantedPlayers)
    in
    Set.foldl
        (\clientId accModel ->
            update
                (BackendMsg (ClientConnected "" clientId))
                accModel
        )
        model
        neededPlayers


suite : Test
suite =
    Test.describe "Marketplace"
        [ Test.describe "Invariant tests"
            [ Test.describe "PutOffer" (suiteForMsg putOfferFuzzer)
            , Test.describe "PutRequest" (suiteForMsg putRequestFuzzer)
            , Test.describe "CancelOffer" (suiteForMsg cancelOfferFuzzer)
            , Test.describe "CancelRequest" (suiteForMsg cancelRequestFuzzer)
            ]
        , Test.describe "Isolated actions"
            [ Test.test "Offer 1x Food 100ea" <|
                \() ->
                    model_
                        |> offer "1" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> playerItemCount "1" Food |> Expect.equal (model_ |> playerItemCount "1" Food |> (\c -> c - 1)) |> Expect.onFail "Player 1 didn't lose an item"
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal (model_ |> marketplaceItemCount Food |> (+) 1) |> Expect.onFail "Item didn't enter the marketplace"
                            ]
            , Test.test "Request 1x Food 100ea" <|
                \() ->
                    model_
                        |> request "1" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 1 |> Expect.onFail "Request didn't enter the marketplace"
                            , \m -> m |> playerMoney "1" |> Expect.equal (model_ |> playerMoney "1" |> (\mm -> mm - 100)) |> Expect.onFail "Player 1 didn't lose money"
                            ]
            , Test.test "Request for more money than we have is no-op" <|
                \() ->
                    model_
                        |> request "1" 1 Food (round (1 / 0))
                        |> expectEquivalent model_
            ]
        , Test.describe "Cancellations"
            [ Test.test "Cancelling a non-existent request is no-op" <|
                \() ->
                    model_
                        |> cancelRequest "1" 1 Food 100
                        |> expectEquivalent model_
            , Test.test "Cancelling a non-existent offer is no-op" <|
                \() ->
                    model_
                        |> cancelOffer "1" 1 Food 100
                        |> expectEquivalent model_
            , Test.test "Request then cancel - same as original state" <|
                \() ->
                    model_
                        |> request "1" 1 Food 100
                        |> cancelRequest "1" 1 Food 100
                        |> expectEquivalent model_
            , Test.test "Offer then cancel - same as original state" <|
                \() ->
                    model_
                        |> offer "1" 1 Food 100
                        |> cancelOffer "1" 1 Food 100
                        |> expectEquivalent model_
            ]
        , Test.describe "2-party transactions"
            [ Test.test "Offer 1x @ 100 then Request 1x @ 100" <|
                \() ->
                    model_
                        |> offer "1" 1 Food 100
                        |> request "2" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> playerItemCount "1" Food |> Expect.equal (model_ |> playerItemCount "1" Food |> (\c -> c - 1)) |> Expect.onFail "Player 1 didn't lose an item"
                            , \m -> m |> playerItemCount "2" Food |> Expect.equal (model_ |> playerItemCount "2" Food |> (+) 1) |> Expect.onFail "Player 2 didn't gain 1 item"
                            , \m -> m |> playerMoney "1" |> Expect.equal (model_ |> playerMoney "1" |> (+) 100) |> Expect.onFail "Player 1 didn't gain money"
                            , \m -> m |> playerMoney "2" |> Expect.equal (model_ |> playerMoney "2" |> (\mm -> mm - 100)) |> Expect.onFail "Player 2 didn't lose money"
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 0 |> Expect.onFail "Item didn't leave the marketplace"
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 0 |> Expect.onFail "Request didn't leave the marketplace"
                            ]
            , Test.test "Offer 1x @ 100 then Request 1x @ 200" <|
                \() ->
                    -- trade takes place at 100
                    model_
                        |> offer "1" 1 Food 100
                        |> request "2" 1 Food 200
                        |> Expect.all
                            [ \m -> m |> playerItemCount "1" Food |> Expect.equal (model_ |> playerItemCount "1" Food |> (\c -> c - 1)) |> Expect.onFail "Player 1 didn't lose an item"
                            , \m -> m |> playerItemCount "2" Food |> Expect.equal (model_ |> playerItemCount "2" Food |> (+) 1) |> Expect.onFail "Player 2 didn't gain 1 item"
                            , \m -> m |> playerMoney "1" |> Expect.equal (model_ |> playerMoney "1" |> (+) 100) |> Expect.onFail "Player 1 didn't gain money"
                            , \m -> m |> playerMoney "2" |> Expect.equal (model_ |> playerMoney "2" |> (\mm -> mm - 100)) |> Expect.onFail "Player 2 didn't lose money"
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 0 |> Expect.onFail "Item didn't leave the marketplace"
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 0 |> Expect.onFail "Request didn't leave the marketplace"
                            ]
            , Test.test "Offer 1x @ 200 then Request 1x @ 100" <|
                \() ->
                    -- no trade takes place
                    model_
                        |> offer "1" 1 Food 200
                        |> request "2" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> playerItemCount "1" Food |> Expect.equal (model_ |> playerItemCount "1" Food |> (\c -> c - 1)) |> Expect.onFail "Player 1 didn't lose an item"
                            , \m -> m |> playerItemCount "2" Food |> Expect.equal (model_ |> playerItemCount "2" Food) |> Expect.onFail "Player 2 didn't keep their item"
                            , \m -> m |> playerMoney "1" |> Expect.equal (model_ |> playerMoney "1") |> Expect.onFail "Player 1 didn't keep their money"
                            , \m -> m |> playerMoney "2" |> Expect.equal (model_ |> playerMoney "2" |> (\mm -> mm - 100)) |> Expect.onFail "Player 2 didn't lose money"
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 1 |> Expect.onFail "Item didn't leave the marketplace"
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 1 |> Expect.onFail "Request didn't leave the marketplace"
                            ]
            , Test.test "Request 1x @ 100 then Offer 1x @ 100" <|
                \() ->
                    model_
                        |> request "2" 1 Food 100
                        |> offer "1" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> playerItemCount "1" Food |> Expect.equal (model_ |> playerItemCount "1" Food |> (\c -> c - 1)) |> Expect.onFail "Player 1 didn't lose an item"
                            , \m -> m |> playerItemCount "2" Food |> Expect.equal (model_ |> playerItemCount "2" Food |> (+) 1) |> Expect.onFail "Player 2 didn't gain 1 item"
                            , \m -> m |> playerMoney "1" |> Expect.equal (model_ |> playerMoney "1" |> (+) 100) |> Expect.onFail "Player 1 didn't gain money"
                            , \m -> m |> playerMoney "2" |> Expect.equal (model_ |> playerMoney "2" |> (\mm -> mm - 100)) |> Expect.onFail "Player 2 didn't lose money"
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 0 |> Expect.onFail "Item didn't leave the marketplace"
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 0 |> Expect.onFail "Request didn't leave the marketplace"
                            ]
            , Test.test "Request 1x @ 200 then Offer 1x @ 100" <|
                \() ->
                    -- trade takes place at 200
                    model_
                        |> request "2" 1 Food 200
                        |> offer "1" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> playerItemCount "1" Food |> Expect.equal (model_ |> playerItemCount "1" Food |> (\c -> c - 1)) |> Expect.onFail "Player 1 didn't lose an item"
                            , \m -> m |> playerItemCount "2" Food |> Expect.equal (model_ |> playerItemCount "2" Food |> (+) 1) |> Expect.onFail "Player 2 didn't gain 1 item"
                            , \m -> m |> playerMoney "1" |> Expect.equal (model_ |> playerMoney "1" |> (+) 200) |> Expect.onFail "Player 1 didn't gain money"
                            , \m -> m |> playerMoney "2" |> Expect.equal (model_ |> playerMoney "2" |> (\mm -> mm - 200)) |> Expect.onFail "Player 2 didn't lose money"
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 0 |> Expect.onFail "Item didn't leave the marketplace"
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 0 |> Expect.onFail "Request didn't leave the marketplace"
                            ]
            , Test.test "Request 1x @ 100 then Offer 1x @ 200" <|
                \() ->
                    -- no trade takes place
                    model_
                        |> request "2" 1 Food 100
                        |> offer "1" 1 Food 200
                        |> Expect.all
                            [ \m -> m |> playerItemCount "1" Food |> Expect.equal (model_ |> playerItemCount "1" Food |> (\c -> c - 1)) |> Expect.onFail "Player 1 didn't lose an item"
                            , \m -> m |> playerItemCount "2" Food |> Expect.equal (model_ |> playerItemCount "2" Food) |> Expect.onFail "Player 2 didn't keep their item"
                            , \m -> m |> playerMoney "1" |> Expect.equal (model_ |> playerMoney "1") |> Expect.onFail "Player 1 didn't keep their money"
                            , \m -> m |> playerMoney "2" |> Expect.equal (model_ |> playerMoney "2" |> (\mm -> mm - 100)) |> Expect.onFail "Player 2 didn't lose money"
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 1 |> Expect.onFail "Item didn't leave the marketplace"
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 1 |> Expect.onFail "Request didn't leave the marketplace"
                            ]
            , Test.test "Use a part of lowest offer" <|
                \() ->
                    model_
                        |> offer "1" 2 Food 100
                        |> request "2" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> playerItemCount "1" Food |> Expect.equal (model_ |> playerItemCount "1" Food |> (\c -> c - 2)) |> Expect.onFail "Player 1 didn't lose 2 items"
                            , \m -> m |> playerItemCount "2" Food |> Expect.equal (model_ |> playerItemCount "2" Food |> (+) 1) |> Expect.onFail "Player 2 didn't gain 1 item"
                            , \m -> m |> playerMoney "1" |> Expect.equal (model_ |> playerMoney "1" |> (+) 100) |> Expect.onFail "Player 1 didn't gain money"
                            , \m -> m |> playerMoney "2" |> Expect.equal (model_ |> playerMoney "2" |> (\mm -> mm - 100)) |> Expect.onFail "Player 2 didn't lose money"
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 1 |> Expect.onFail "Item didn't leave the marketplace"
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 0 |> Expect.onFail "Request didn't leave the marketplace"
                            ]
            ]
        , Test.describe "3-party transactions"
            [ Test.describe "Offer 1x @ 100, Offer 1x @ 100, Request 2x @ 100 - everything gets sold"
                (List.Extra.permutations
                    [ ( "1", offer "1" 1 Food 100 )
                    , ( "2", offer "2" 1 Food 100 )
                    , ( "3", request "3" 2 Food 100 )
                    ]
                    |> List.map
                        (\permutation ->
                            let
                                ( names, fns ) =
                                    List.unzip permutation
                            in
                            Test.test (String.join "," names) <|
                                \() ->
                                    List.foldl
                                        (\fn accModel -> fn accModel)
                                        model_
                                        fns
                                        |> Expect.all
                                            [ \m -> m |> playerItemCount "1" Food |> Expect.equal (model_ |> playerItemCount "1" Food |> (\c -> c - 1)) |> Expect.onFail "Player 1 didn't lose an item"
                                            , \m -> m |> playerItemCount "2" Food |> Expect.equal (model_ |> playerItemCount "2" Food |> (\c -> c - 1)) |> Expect.onFail "Player 2 didn't lose an item"
                                            , \m -> m |> playerItemCount "3" Food |> Expect.equal (model_ |> playerItemCount "3" Food |> (+) 2) |> Expect.onFail "Player 3 didn't gain 2 items"
                                            , \m -> m |> playerMoney "1" |> Expect.equal (model_ |> playerMoney "1" |> (+) 100) |> Expect.onFail "Player 1 didn't gain money"
                                            , \m -> m |> playerMoney "2" |> Expect.equal (model_ |> playerMoney "2" |> (+) 100) |> Expect.onFail "Player 2 didn't gain money"
                                            , \m -> m |> playerMoney "3" |> Expect.equal (model_ |> playerMoney "3" |> (\mm -> mm - 200)) |> Expect.onFail "Player 3 didn't lose money"
                                            ]
                        )
                )
            ]
        , Test.describe "Liveness tests"
            [ ATest.invariantTest "if an offer exists for N, a request for N will succeed" testedApp <|
                \_ _ newModel ->
                    let
                        newModelWithPlayers =
                            newModel
                                |> guaranteePlayers
                                |> guaranteeEnoughMoney
                    in
                    case anyOffer newModelWithPlayers of
                        Nothing ->
                            Expect.pass

                        Just ( kind, offer_ ) ->
                            let
                                seller =
                                    offer_.clientId

                                buyer =
                                    otherClientId seller

                                count =
                                    List.length offer_.ids
                            in
                            newModelWithPlayers
                                |> request buyer count kind offer_.unitPrice
                                |> Expect.all
                                    [ \m -> m |> playerItemCount buyer kind |> Expect.equal (newModelWithPlayers |> playerItemCount buyer kind |> (+) count) |> Expect.onFail "Buyer didn't receive the item"
                                    , \m -> m |> playerMoney buyer |> Expect.equal (newModelWithPlayers |> playerMoney buyer |> (\mm -> mm - (count * offer_.unitPrice))) |> Expect.onFail "Buyer didn't pay for the item"
                                    , \m -> m |> marketplaceItemCount kind |> Expect.equal (newModelWithPlayers |> marketplaceItemCount kind |> (\c -> c - count)) |> Expect.onFail "Item didn't leave the marketplace"
                                    , \m -> m |> playerMoney seller |> Expect.equal (newModelWithPlayers |> playerMoney seller |> (+) (count * offer_.unitPrice)) |> Expect.onFail "Seller didn't receive money"
                                    ]
            , ATest.invariantTest "if an offer exists for N, a request for N+1 will succeed for N" testedApp <|
                \_ _ newModel ->
                    let
                        newModelWithPlayers =
                            newModel
                                |> guaranteePlayers
                                |> guaranteeEnoughMoney
                    in
                    case anyOffer newModelWithPlayers of
                        Nothing ->
                            Expect.pass

                        Just ( kind, offer_ ) ->
                            let
                                seller =
                                    offer_.clientId

                                buyer =
                                    otherClientId seller

                                count =
                                    List.length offer_.ids
                            in
                            newModelWithPlayers
                                |> request buyer count kind (offer_.unitPrice + 1)
                                |> Expect.all
                                    [ \m -> m |> playerItemCount buyer kind |> Expect.equal (newModelWithPlayers |> playerItemCount buyer kind |> (+) count) |> Expect.onFail "Buyer didn't receive the item"
                                    , \m -> m |> playerMoney buyer |> Expect.equal (newModelWithPlayers |> playerMoney buyer |> (\mm -> mm - (count * offer_.unitPrice))) |> Expect.onFail "Buyer didn't pay for the item"
                                    , \m -> m |> marketplaceItemCount kind |> Expect.equal (newModelWithPlayers |> marketplaceItemCount kind |> (\c -> c - count)) |> Expect.onFail "Item didn't leave the marketplace"
                                    , \m -> m |> playerMoney seller |> Expect.equal (newModelWithPlayers |> playerMoney seller |> (+) (count * offer_.unitPrice)) |> Expect.onFail "Seller didn't receive money"
                                    ]
            , ATest.invariantTest "if a request exists for N, an offer for N will succeed" testedApp <|
                \_ _ newModel ->
                    case anyRequest newModel of
                        Nothing ->
                            Expect.pass

                        Just ( kind, request_ ) ->
                            let
                                buyer =
                                    request_.clientId

                                seller =
                                    otherClientId buyer

                                count =
                                    request_.count

                                newModelWithPlayers =
                                    newModel
                                        |> guaranteePlayers
                                        |> sudoAddItems count kind seller
                            in
                            newModelWithPlayers
                                |> offer seller count kind request_.unitPrice
                                |> Expect.all
                                    [ \m -> m |> playerItemCount seller kind |> Expect.equal (newModelWithPlayers |> playerItemCount seller kind |> (\c -> c - count)) |> Expect.onFail "Seller didn't lose the item"
                                    , \m -> m |> playerMoney seller |> Expect.equal (newModelWithPlayers |> playerMoney seller |> (+) (count * request_.unitPrice)) |> Expect.onFail "Seller didn't receive money"
                                    , \m -> m |> marketplaceItemRequestCount kind |> Expect.equal (newModelWithPlayers |> marketplaceItemRequestCount kind |> (\c -> c - count)) |> Expect.onFail "Item request didn't leave the marketplace"
                                    , \m -> m |> playerItemCount buyer kind |> Expect.equal (newModelWithPlayers |> playerItemCount buyer kind |> (+) count) |> Expect.onFail "Buyer didn't get the item"
                                    ]
            , ATest.invariantTest "if a request exists for N money, an offer for N-1 money will succeed for N money" testedApp <|
                \_ _ newModel ->
                    case anyRequest newModel of
                        Nothing ->
                            Expect.pass

                        Just ( kind, request_ ) ->
                            if request_.unitPrice == 1 then
                                Expect.pass

                            else
                                let
                                    buyer =
                                        request_.clientId

                                    seller =
                                        otherClientId buyer

                                    count =
                                        request_.count

                                    unitPrice =
                                        request_.unitPrice

                                    newModelWithPlayers =
                                        newModel
                                            |> guaranteePlayers
                                            |> sudoAddItems count kind seller
                                in
                                newModelWithPlayers
                                    |> offer seller count kind (unitPrice - 1)
                                    |> Expect.all
                                        [ \m -> m |> playerItemCount seller kind |> Expect.equal (newModelWithPlayers |> playerItemCount seller kind |> (\c -> c - count)) |> Expect.onFail "Seller didn't lose the item"
                                        , \m -> m |> playerMoney seller |> Expect.equal (newModelWithPlayers |> playerMoney seller |> (+) (count * unitPrice)) |> Expect.onFail "Seller didn't receive money"
                                        , \m -> m |> marketplaceItemRequestCount kind |> Expect.equal (newModelWithPlayers |> marketplaceItemRequestCount kind |> (\c -> c - count)) |> Expect.onFail "Item request didn't leave the marketplace"
                                        , \m -> m |> playerItemCount buyer kind |> Expect.equal (newModelWithPlayers |> playerItemCount buyer kind |> (+) count) |> Expect.onFail "Buyer didn't get the item"
                                        ]
            ]

        {- , Test.describe "Safety tests"
           [ ATest.invariantTest "if the lowest offer is for N, a request for N-1 will not succeed" testedApp <|
               \() ->
                   ()
           , ATest.invariantTest "if the highest request is for N, an offer for N+1 will not succeed" testedApp <|
               \() ->
                   ()
           ]
        -}
        ]


guaranteeEnoughMoney : BackendModel -> BackendModel
guaranteeEnoughMoney model =
    { model
        | players =
            model.players
                |> Dict.map (\_ p -> { p | money = 1000000 })
    }


sudoAddItems : Int -> ItemKind -> ClientId -> BackendModel -> BackendModel
sudoAddItems count itemKind clientId model =
    let
        addedItems =
            List.range model.nextId (model.nextId + count - 1)
    in
    { model
        | players =
            model.players
                |> Dict.update clientId
                    (Maybe.map
                        (\p ->
                            { p
                                | items =
                                    p.items
                                        |> SeqDict.update itemKind
                                            (\maybeIds ->
                                                case maybeIds of
                                                    Nothing ->
                                                        Just addedItems

                                                    Just ids ->
                                                        Just (ids ++ addedItems)
                                            )
                            }
                        )
                    )
        , nextId = model.nextId + count
    }


otherClientId : ClientId -> ClientId
otherClientId clientId =
    if clientId == "1" then
        "2"

    else
        "1"


anyOffer : BackendModel -> Maybe ( ItemKind, { clientId : ClientId, ids : List Id, unitPrice : Int } )
anyOffer model =
    model.marketplace
        |> SeqDict.toList
        |> List.Extra.findMap
            (\( kind, market ) ->
                MinPriorityQueue.smallest market.offers
                    |> Maybe.map (Tuple.pair kind)
            )


anyRequest : BackendModel -> Maybe ( ItemKind, { clientId : ClientId, count : Int, unitPrice : Int } )
anyRequest model =
    model.marketplace
        |> SeqDict.toList
        |> List.Extra.findMap
            (\( kind, market ) ->
                MaxPriorityQueue.largest market.requests
                    |> Maybe.map (Tuple.pair kind)
            )


suiteForMsg : Fuzzer ToBackend -> List Test
suiteForMsg msgFuzzer_ =
    [ ATest.msgTest "item count constant" testedApp (Fuzz.map2 ToBackendMsg clientIdFuzzer msgFuzzer_) <|
        \oldModel msg newModel ->
            totalItemsCount newModel |> Expect.equal (totalItemsCount oldModel)
    , ATest.msgTest "money amount constant" testedApp (Fuzz.map2 ToBackendMsg clientIdFuzzer msgFuzzer_) <|
        \oldModel msg newModel ->
            totalMoneyAmount newModel |> Expect.equal (totalMoneyAmount oldModel)
    ]


testedApp : TestedApp BackendModel Msg
testedApp =
    { model = ATest.ConstantModel Backend.initModel
    , update = ATest.UpdateWithoutCmds update
    , msgFuzzer = msgFuzzer
    , msgToString = Debug.toString
    , modelToString = Debug.toString
    }


update : Msg -> BackendModel -> BackendModel
update msg model =
    case msg of
        BackendMsg backendMsg ->
            Backend.update backendMsg model
                |> Tuple.first

        ToBackendMsg clientId toBackendMsg ->
            Backend.updateFromFrontend "" clientId toBackendMsg model
                |> Tuple.first


msgFuzzer : Fuzzer Msg
msgFuzzer =
    Fuzz.frequency
        [ ( 1, Fuzz.map BackendMsg backendMsgFuzzer )
        , ( 10, Fuzz.map2 ToBackendMsg clientIdFuzzer toBackendFuzzer )
        ]


backendMsgFuzzer : Fuzzer BackendMsg
backendMsgFuzzer =
    Fuzz.oneOf
        [ Fuzz.map (ClientConnected "") clientIdFuzzer
        , Fuzz.map (ClientDisconnected "") clientIdFuzzer
        ]


toBackendFuzzer : Fuzzer ToBackend
toBackendFuzzer =
    Fuzz.oneOf
        [ putOfferFuzzer
        , putRequestFuzzer
        , cancelOfferFuzzer
        , cancelRequestFuzzer
        ]


clientIdFuzzer : Fuzzer Lamdera.ClientId
clientIdFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map String.fromInt


offerFuzzer : Fuzzer Offer
offerFuzzer =
    Fuzz.map3 Offer
        itemKindFuzzer
        unitPriceFuzzer
        (uniqueListFuzzer itemIdFuzzer)


uniqueListFuzzer : Fuzzer comparable -> Fuzzer (List comparable)
uniqueListFuzzer itemFuzzer =
    Fuzz.listOfLengthBetween 1 3 itemFuzzer
        |> Fuzz.map (Set.fromList >> Set.toList)


requestFuzzer : Fuzzer Request
requestFuzzer =
    Fuzz.map3 Request
        itemKindFuzzer
        unitPriceFuzzer
        itemCountFuzzer


unitPriceFuzzer : Fuzzer Int
unitPriceFuzzer =
    Fuzz.intRange 1 100


itemCountFuzzer : Fuzzer Int
itemCountFuzzer =
    Fuzz.intRange 1 5


itemIdFuzzer : Fuzzer Id
itemIdFuzzer =
    -- TODO make this somehow depend on the model
    Fuzz.intRange 1 50


itemKindFuzzer : Fuzzer ItemKind
itemKindFuzzer =
    Fuzz.oneOfValues
        [ Gun
        , Ammo
        , Armor
        , Food
        ]


putOfferFuzzer : Fuzzer ToBackend
putOfferFuzzer =
    Fuzz.map PutOffer offerFuzzer


putRequestFuzzer : Fuzzer ToBackend
putRequestFuzzer =
    Fuzz.map PutRequest requestFuzzer


cancelOfferFuzzer : Fuzzer ToBackend
cancelOfferFuzzer =
    Fuzz.map CancelOffer offerFuzzer


cancelRequestFuzzer : Fuzzer ToBackend
cancelRequestFuzzer =
    Fuzz.map CancelRequest requestFuzzer


offer : ClientId -> Int -> ItemKind -> Int -> BackendModel -> BackendModel
offer clientId count itemKind unitPrice model =
    let
        ids =
            playerItemIds clientId itemKind model
    in
    if count > List.length ids then
        Debug.todo "[offer] Test tried to offer more items than the player got"

    else
        model
            |> update
                (ToBackendMsg clientId
                    (PutOffer
                        { kind = itemKind
                        , unitPrice = unitPrice
                        , ids = List.take count ids
                        }
                    )
                )


request : ClientId -> Int -> ItemKind -> Int -> BackendModel -> BackendModel
request clientId count itemKind unitPrice model =
    model
        |> update
            (ToBackendMsg clientId
                (PutRequest
                    { kind = itemKind
                    , unitPrice = unitPrice
                    , count = count
                    }
                )
            )


cancelRequest : ClientId -> Int -> ItemKind -> Int -> BackendModel -> BackendModel
cancelRequest clientId count itemKind unitPrice model =
    model
        |> update
            (ToBackendMsg clientId
                (CancelRequest
                    { kind = itemKind
                    , unitPrice = unitPrice
                    , count = count
                    }
                )
            )


cancelOffer : ClientId -> Int -> ItemKind -> Int -> BackendModel -> BackendModel
cancelOffer clientId count itemKind unitPrice model =
    let
        ids =
            model.marketplace
                |> SeqDict.get itemKind
                |> Maybe.andThen
                    (.offers
                        >> MinPriorityQueue.toList
                        >> List.Extra.find
                            (\offer_ ->
                                (offer_.clientId == clientId)
                                    && (offer_.unitPrice == unitPrice)
                                    && (List.length offer_.ids == count)
                            )
                    )
                |> Maybe.map (.ids >> List.take count)
                |> Maybe.withDefault []
    in
    model
        |> update
            (ToBackendMsg clientId
                (CancelOffer
                    { kind = itemKind
                    , unitPrice = unitPrice
                    , ids = ids
                    }
                )
            )


playerMoney : ClientId -> BackendModel -> Int
playerMoney clientId model =
    model.players
        |> Dict.get clientId
        |> Maybe.map .money
        |> Maybe.withDefault 0


playerItemIds : ClientId -> ItemKind -> BackendModel -> List Id
playerItemIds clientId itemKind model =
    model.players
        |> Dict.get clientId
        |> Maybe.andThen (.items >> SeqDict.get itemKind)
        |> Maybe.withDefault []


playerItemCount : ClientId -> ItemKind -> BackendModel -> Int
playerItemCount clientId itemKind model =
    playerItemIds clientId itemKind model
        |> List.length


marketplaceItemCount : ItemKind -> BackendModel -> Int
marketplaceItemCount itemKind model =
    model.marketplace
        |> SeqDict.get itemKind
        |> Maybe.map (.offers >> MinPriorityQueue.toList >> List.map (.ids >> List.length) >> List.sum)
        |> Maybe.withDefault 0


marketplaceItemRequestCount : ItemKind -> BackendModel -> Int
marketplaceItemRequestCount itemKind model =
    model.marketplace
        |> SeqDict.get itemKind
        |> Maybe.map (.requests >> MaxPriorityQueue.toList >> List.map .count >> List.sum)
        |> Maybe.withDefault 0


marketplaceTotalItemsCount : BackendModel -> Int
marketplaceTotalItemsCount model =
    model.marketplace
        |> SeqDict.values
        |> List.map
            (.offers
                >> MinPriorityQueue.toList
                >> List.map (.ids >> List.length)
                >> List.sum
            )
        |> List.sum


playersTotalItemsCount : BackendModel -> Int
playersTotalItemsCount model =
    model.players
        |> Dict.values
        |> List.concatMap (.items >> SeqDict.values)
        |> List.map List.length
        |> List.sum


totalItemsCount : BackendModel -> Int
totalItemsCount model =
    marketplaceTotalItemsCount model
        + playersTotalItemsCount model


marketplaceTotalMoneyAmount : BackendModel -> Int
marketplaceTotalMoneyAmount model =
    model.marketplace
        |> SeqDict.values
        |> List.map
            (.requests
                >> MaxPriorityQueue.toList
                >> List.map (\req -> req.count * req.unitPrice)
                >> List.sum
            )
        |> List.sum


playersTotalMoneyAmount : BackendModel -> Int
playersTotalMoneyAmount model =
    model.players
        |> Dict.values
        |> List.map .money
        |> List.sum


totalMoneyAmount : BackendModel -> Int
totalMoneyAmount model =
    marketplaceTotalMoneyAmount model
        + playersTotalMoneyAmount model


expectEquivalent : BackendModel -> BackendModel -> Expectation
expectEquivalent old new =
    new
        |> Expect.all
            [ \m -> marketplaceTotalItemsCount m |> Expect.equal (marketplaceTotalItemsCount old) |> Expect.onFail "Marketplace items count mismatch"
            , \m -> marketplaceTotalMoneyAmount m |> Expect.equal (marketplaceTotalMoneyAmount old) |> Expect.onFail "Marketplace money amount mismatch"
            , \m -> playersTotalItemsCount m |> Expect.equal (playersTotalItemsCount old) |> Expect.onFail "Players items count mismatch"
            , \m -> playersTotalMoneyAmount m |> Expect.equal (playersTotalMoneyAmount old) |> Expect.onFail "Players money amount mismatch"
            ]
