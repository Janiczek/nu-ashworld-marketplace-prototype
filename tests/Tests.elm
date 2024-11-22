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
        |> update (BackendMsg (ClientConnected "" "A"))
        |> update (BackendMsg (ClientConnected "" "B"))
        |> update (BackendMsg (ClientConnected "" "C"))
        |> update (BackendMsg (ClientConnected "" "D"))
        |> update (BackendMsg (ClientConnected "" "E"))


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
                        |> offer "A" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> playerItemCount "A" Food |> Expect.equal (model_ |> playerItemCount "A" Food |> (\c -> c - 1))
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal (model_ |> marketplaceItemCount Food |> (+) 1)
                            ]
            , Test.test "Request 1x Food 100ea" <|
                \() ->
                    model_
                        |> request "A" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 1
                            , \m -> m |> playerMoney "A" |> Expect.equal (model_ |> playerMoney "A" |> (\mm -> mm - 100))
                            ]
            ]
        , Test.describe "Cancellations"
            [ Test.test "Request then cancel - same as original state" <|
                \() ->
                    model_
                        |> request "A" 1 Food 100
                        |> cancelRequest "A" 1 Food 100
                        |> expectEquivalent model_
            , Test.test "Offer then cancel - same as original state" <|
                \() ->
                    model_
                        |> offer "A" 1 Food 100
                        |> cancelOffer "A" 1 Food 100
                        |> expectEquivalent model_
            ]
        , Test.describe "2-party transactions"
            [ Test.test "Offer 1x @ 100 then Request 1x @ 100" <|
                \() ->
                    model_
                        |> offer "A" 1 Food 100
                        |> request "B" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> playerItemCount "A" Food |> Expect.equal (model_ |> playerItemCount "A" Food |> (\c -> c - 1))
                            , \m -> m |> playerItemCount "B" Food |> Expect.equal (model_ |> playerItemCount "B" Food |> (+) 1)
                            , \m -> m |> playerMoney "A" |> Expect.equal (model_ |> playerMoney "A" |> (+) 100)
                            , \m -> m |> playerMoney "B" |> Expect.equal (model_ |> playerMoney "B" |> (\mm -> mm - 100))
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 0
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 0
                            ]
            , Test.test "Offer 1x @ 100 then Request 1x @ 200" <|
                \() ->
                    -- trade takes place at 100
                    model_
                        |> offer "A" 1 Food 100
                        |> request "B" 1 Food 200
                        |> Expect.all
                            [ \m -> m |> playerItemCount "A" Food |> Expect.equal (model_ |> playerItemCount "A" Food |> (\c -> c - 1))
                            , \m -> m |> playerItemCount "B" Food |> Expect.equal (model_ |> playerItemCount "B" Food |> (+) 1)
                            , \m -> m |> playerMoney "A" |> Expect.equal (model_ |> playerMoney "A" |> (+) 100)
                            , \m -> m |> playerMoney "B" |> Expect.equal (model_ |> playerMoney "B" |> (\mm -> mm - 100))
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 0
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 0
                            ]
            , Test.test "Offer 1x @ 200 then Request 1x @ 100" <|
                \() ->
                    -- no trade takes place
                    model_
                        |> offer "A" 1 Food 200
                        |> request "B" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> playerItemCount "A" Food |> Expect.equal (model_ |> playerItemCount "A" Food |> (\c -> c - 1))
                            , \m -> m |> playerItemCount "B" Food |> Expect.equal (model_ |> playerItemCount "B" Food)
                            , \m -> m |> playerMoney "A" |> Expect.equal (model_ |> playerMoney "A")
                            , \m -> m |> playerMoney "B" |> Expect.equal (model_ |> playerMoney "B" |> (\mm -> mm - 100))
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 1
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 1
                            ]
            , Test.test "Request 1x @ 100 then Offer 1x @ 100" <|
                \() ->
                    model_
                        |> request "B" 1 Food 100
                        |> offer "A" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> playerItemCount "A" Food |> Expect.equal (model_ |> playerItemCount "A" Food |> (\c -> c - 1))
                            , \m -> m |> playerItemCount "B" Food |> Expect.equal (model_ |> playerItemCount "B" Food |> (+) 1)
                            , \m -> m |> playerMoney "A" |> Expect.equal (model_ |> playerMoney "A" |> (+) 100)
                            , \m -> m |> playerMoney "B" |> Expect.equal (model_ |> playerMoney "B" |> (\mm -> mm - 100))
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 0
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 0
                            ]
            , Test.test "Request 1x @ 200 then Offer 1x @ 100" <|
                \() ->
                    -- trade takes place at 200
                    model_
                        |> request "B" 1 Food 200
                        |> offer "A" 1 Food 100
                        |> Expect.all
                            [ \m -> m |> playerItemCount "A" Food |> Expect.equal (model_ |> playerItemCount "A" Food |> (\c -> c - 1))
                            , \m -> m |> playerItemCount "B" Food |> Expect.equal (model_ |> playerItemCount "B" Food |> (+) 1)
                            , \m -> m |> playerMoney "A" |> Expect.equal (model_ |> playerMoney "A" |> (+) 200)
                            , \m -> m |> playerMoney "B" |> Expect.equal (model_ |> playerMoney "B" |> (\mm -> mm - 200))
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 0
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 0
                            ]
            , Test.test "Request 1x @ 100 then Offer 1x @ 200" <|
                \() ->
                    -- no trade takes place
                    model_
                        |> request "B" 1 Food 100
                        |> offer "A" 1 Food 200
                        |> Expect.all
                            [ \m -> m |> playerItemCount "A" Food |> Expect.equal (model_ |> playerItemCount "A" Food |> (\c -> c - 1))
                            , \m -> m |> playerItemCount "B" Food |> Expect.equal (model_ |> playerItemCount "B" Food)
                            , \m -> m |> playerMoney "A" |> Expect.equal (model_ |> playerMoney "A")
                            , \m -> m |> playerMoney "B" |> Expect.equal (model_ |> playerMoney "B" |> (\mm -> mm - 100))
                            , \m -> m |> marketplaceItemCount Food |> Expect.equal 1
                            , \m -> m |> marketplaceItemRequestCount Food |> Expect.equal 1
                            ]
            ]
        , Test.describe "3-party transactions"
            [ Test.describe "Offer 1x @ 100, Offer 1x @ 100, Request 2x @ 100 - everything gets sold"
                (List.Extra.permutations
                    [ ( "A", offer "A" 1 Food 100 )
                    , ( "B", offer "B" 1 Food 100 )
                    , ( "C", request "C" 2 Food 100 )
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
                                            [ \m -> m |> playerItemCount "A" Food |> Expect.equal (model_ |> playerItemCount "A" Food |> (\c -> c - 1))
                                            , \m -> m |> playerItemCount "B" Food |> Expect.equal (model_ |> playerItemCount "B" Food |> (\c -> c - 1))
                                            , \m -> m |> playerItemCount "C" Food |> Expect.equal (model_ |> playerItemCount "C" Food |> (+) 2)
                                            , \m -> m |> playerMoney "A" |> Expect.equal (model_ |> playerMoney "A" |> (+) 100)
                                            , \m -> m |> playerMoney "B" |> Expect.equal (model_ |> playerMoney "A" |> (+) 100)
                                            , \m -> m |> playerMoney "C" |> Expect.equal (model_ |> playerMoney "B" |> (\mm -> mm - 200))
                                            ]
                        )
                )
            ]
        ]


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
    Fuzz.list itemFuzzer
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
    let
        money =
            playerMoney clientId model
    in
    if money < unitPrice * count then
        Debug.todo "[offer] Test tried to request things the player didn't have money for"

    else
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
    in
    case ids of
        Nothing ->
            Debug.todo "[offer] Test tried to cancel offer that didn't exist"

        Just ids_ ->
            model
                |> update
                    (ToBackendMsg clientId
                        (CancelOffer
                            { kind = itemKind
                            , unitPrice = unitPrice
                            , ids = ids_
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
            [ \m -> marketplaceTotalItemsCount m |> Expect.equal (marketplaceTotalItemsCount old)
            , \m -> marketplaceTotalMoneyAmount m |> Expect.equal (marketplaceTotalMoneyAmount old)
            , \m -> playersTotalItemsCount m |> Expect.equal (playersTotalItemsCount old)
            , \m -> playersTotalMoneyAmount m |> Expect.equal (playersTotalMoneyAmount old)
            ]
