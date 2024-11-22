module Backend exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import MaxPriorityQueue
import MinPriorityQueue
import SeqDict exposing (SeqDict)
import Types exposing (..)


type alias Model =
    BackendModel


cmdAndThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
cmdAndThen f ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            f model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { marketplace = SeqDict.empty
      , players = Dict.empty
      , nextId = 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]


initPlayer : Model -> ( { money : Int, items : SeqDict ItemKind (List Id) }, Model )
initPlayer model =
    ( { money = 1000
      , items =
            SeqDict.fromList
                [ ( Gun, [ model.nextId + 0, model.nextId + 1 ] )
                , ( Ammo, [ model.nextId + 2 ] )
                , ( Armor, [ model.nextId + 3 ] )
                , ( Food, [ model.nextId + 4, model.nextId + 5, model.nextId + 6 ] )
                ]
      }
    , { model | nextId = model.nextId + 7 }
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected _ clientId ->
            let
                ( newPlayer, newModel ) =
                    initPlayer model
            in
            ( { newModel | players = Dict.insert clientId newPlayer model.players }
            , Lamdera.sendToFrontend clientId
                (YourBelongings
                    { offers = []
                    , requests = []
                    , items = newPlayer.items
                    , money = newPlayer.money
                    }
                )
            )

        ClientDisconnected _ clientId ->
            let
                updatedMarketplace =
                    SeqDict.map
                        (\_ market ->
                            { offers = MinPriorityQueue.filter (\offer -> offer.clientId /= clientId) market.offers
                            , requests = MaxPriorityQueue.filter (\request -> request.clientId /= clientId) market.requests
                            }
                        )
                        model.marketplace

                updatedPlayers =
                    Dict.remove clientId model.players
            in
            ( { model
                | players = updatedPlayers
                , marketplace = updatedMarketplace
              }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        PutOffer offer ->
            putOffer offer clientId model

        PutRequest request ->
            putRequest request clientId model

        CancelOffer offer ->
            cancelOffer offer clientId model

        CancelRequest request ->
            cancelRequest request clientId model


cancelRequest : Request -> ClientId -> Model -> ( Model, Cmd BackendMsg )
cancelRequest { kind, unitPrice, count } clientId model =
    let
        updatedMarketplace =
            SeqDict.update kind
                (Maybe.map
                    (\market ->
                        { market
                            | requests =
                                MaxPriorityQueue.filter
                                    (\request ->
                                        not (request.clientId == clientId && request.unitPrice == unitPrice && request.count == count)
                                    )
                                    market.requests
                        }
                    )
                )
                model.marketplace

        playerMoney : Int
        playerMoney =
            moneyForPlayer clientId model.players

        newPlayerMoney : Int
        newPlayerMoney =
            playerMoney + (count * unitPrice)

        updatedPlayers : Dict ClientId Player
        updatedPlayers =
            Dict.update clientId
                (Maybe.map
                    (\player ->
                        { player | money = newPlayerMoney }
                    )
                )
                model.players

        newModel =
            { model
                | marketplace = updatedMarketplace
                , players = updatedPlayers
            }
    in
    ( newModel
    , Lamdera.sendToFrontend clientId
        (YouCanceledRequest
            { kind = kind
            , count = count
            , unitPrice = unitPrice
            , newRequests = requestsForPlayer clientId newModel.marketplace
            , newMoney = newPlayerMoney
            }
        )
    )


cancelOffer : Offer -> ClientId -> Model -> ( Model, Cmd BackendMsg )
cancelOffer { kind, unitPrice, ids } clientId model =
    let
        updatedMarketplace =
            SeqDict.update kind
                (Maybe.map
                    (\market ->
                        { market
                            | offers =
                                MinPriorityQueue.filter
                                    (\offer ->
                                        not (offer.clientId == clientId && offer.unitPrice == unitPrice && offer.ids == ids)
                                    )
                                    market.offers
                        }
                    )
                )
                model.marketplace

        playerItems : SeqDict ItemKind (List Id)
        playerItems =
            itemsForPlayer clientId model.players

        newPlayerItems : SeqDict ItemKind (List Id)
        newPlayerItems =
            playerItems
                |> SeqDict.update kind
                    (\oldIds ->
                        case oldIds of
                            Nothing ->
                                Just ids

                            Just oldIds_ ->
                                Just (oldIds_ ++ ids)
                    )

        updatedPlayers : Dict ClientId Player
        updatedPlayers =
            Dict.update clientId
                (Maybe.map
                    (\player ->
                        { player | items = newPlayerItems }
                    )
                )
                model.players

        newModel =
            { model
                | marketplace = updatedMarketplace
                , players = updatedPlayers
            }
    in
    ( newModel
    , Lamdera.sendToFrontend clientId
        (YouCanceledOffer
            { kind = kind
            , count = List.length ids
            , unitPrice = unitPrice
            , newOffers = offersForPlayer clientId newModel.marketplace
            , newItems = newPlayerItems
            }
        )
    )


putRequest : Request -> ClientId -> Model -> ( Model, Cmd BackendMsg )
putRequest ({ kind, unitPrice, count } as request) clientId model =
    let
        playerMoney : Int
        playerMoney =
            moneyForPlayer clientId model.players

        totalPrice : Int
        totalPrice =
            unitPrice * count
    in
    if totalPrice > playerMoney then
        -- Ignore the request, not enough money
        ( model, Cmd.none )

    else
        let
            remainingMoney : Int
            remainingMoney =
                playerMoney - totalPrice

            updatedPlayers : Dict ClientId Player
            updatedPlayers =
                Dict.update clientId
                    (Maybe.map (\player -> { player | money = remainingMoney }))
                    model.players

            newModel =
                { model | players = updatedPlayers }

            tryToMatch :
                Int
                -> List { ids : List Id, unitPrice : Int }
                -> Int
                -> Model
                -> List (Model -> Cmd BackendMsg)
                -> ( Model, Cmd BackendMsg )
            tryToMatch itemsToBuy accBoughtItems accExtraMoney accModel accToCmds =
                case SeqDict.get kind accModel.marketplace of
                    -- No offers for this item kind!
                    Nothing ->
                        finishRequest itemsToBuy accBoughtItems accExtraMoney accModel accToCmds

                    Just { offers } ->
                        case MinPriorityQueue.smallest offers of
                            -- No offers for this item kind!
                            Nothing ->
                                finishRequest itemsToBuy accBoughtItems accExtraMoney accModel accToCmds

                            -- We found some offers for this item kind. Let's check if we can meet any of them.
                            Just lowestOffer ->
                                let
                                    itemsFromOffer : List Id
                                    itemsFromOffer =
                                        lowestOffer.ids

                                    offerItemsCount : Int
                                    offerItemsCount =
                                        List.length itemsFromOffer
                                in
                                if unitPrice < lowestOffer.unitPrice then
                                    -- I'm buying for less than the lowest offer is willing to sell for.
                                    -- Can't use this offer, and it was our best bet - any other offer
                                    -- won't be any better. End here.
                                    finishRequest itemsToBuy accBoughtItems accExtraMoney accModel accToCmds

                                else
                                    -- Let's do the exchange.
                                    let
                                        itemsBoughtCount : Int
                                        itemsBoughtCount =
                                            min offerItemsCount itemsToBuy

                                        idsBought : List Id
                                        idsBought =
                                            List.take itemsBoughtCount lowestOffer.ids

                                        finalPrice : Int
                                        finalPrice =
                                            lowestOffer.unitPrice

                                        priceDifference : Int
                                        priceDifference =
                                            -- I'm willing to pay 300
                                            -- They're willing to sell for 200
                                            -- I get 100 back
                                            unitPrice - finalPrice

                                        totalPriceDifference : Int
                                        totalPriceDifference =
                                            priceDifference * itemsBoughtCount

                                        toSellerCmd : Model -> Cmd BackendMsg
                                        toSellerCmd m =
                                            Lamdera.sendToFrontend lowestOffer.clientId
                                                (YouSold
                                                    { kind = kind
                                                    , count = itemsBoughtCount
                                                    , unitPrice = finalPrice
                                                    , newOffers = offersForPlayer lowestOffer.clientId m.marketplace
                                                    , newMoney = moneyForPlayer lowestOffer.clientId m.players
                                                    }
                                                )

                                        newAccModel =
                                            accModel
                                                |> removeFromLowestOffer kind idsBought
                                                |> updatePlayer lowestOffer.clientId
                                                    (\player -> { player | money = player.money + finalPrice * itemsBoughtCount })
                                    in
                                    tryToMatch
                                        (itemsToBuy - itemsBoughtCount)
                                        ({ ids = idsBought, unitPrice = finalPrice } :: accBoughtItems)
                                        (accExtraMoney + totalPriceDifference)
                                        newAccModel
                                        (toSellerCmd :: accToCmds)

            finishRequest :
                Int
                -> List { ids : List Id, unitPrice : Int }
                -> Int
                -> Model
                -> List (Model -> Cmd BackendMsg)
                -> ( Model, Cmd BackendMsg )
            finishRequest itemsToBuy accBoughtItems accExtraMoney accModel accToCmds =
                let
                    modelWithRequestAdded =
                        if itemsToBuy == 0 then
                            -- Everything insta-bought!
                            -- We never added the request, so we don't need to remove it.
                            -- We've removed/updated the matched offers, so we don't need to do that here.
                            accModel

                        else
                            let
                                -- Leftover request that couldn't be met by the current offers
                                finalRequest =
                                    { clientId = clientId
                                    , count = itemsToBuy
                                    , unitPrice = unitPrice
                                    }
                            in
                            accModel
                                |> addRequest kind finalRequest

                    modelWithItemsAndLeftoverMoneyAdded =
                        modelWithRequestAdded
                            |> updatePlayer clientId
                                (\player ->
                                    { money = player.money + accExtraMoney
                                    , items =
                                        player.items
                                            |> addItems kind (List.concatMap .ids accBoughtItems)
                                    }
                                )
                in
                ( modelWithItemsAndLeftoverMoneyAdded
                , Cmd.batch <|
                    Lamdera.sendToFrontend clientId
                        (YouRequested
                            { kind = kind
                            , totalCount = count
                            , unitPrice = unitPrice
                            , newRequests = requestsForPlayer clientId modelWithItemsAndLeftoverMoneyAdded.marketplace
                            , newMoney = moneyForPlayer clientId modelWithItemsAndLeftoverMoneyAdded.players
                            , newItems = itemsForPlayer clientId modelWithItemsAndLeftoverMoneyAdded.players
                            , outcome =
                                if itemsToBuy == count then
                                    RequestedButNotMet

                                else if itemsToBuy == 0 then
                                    RequestedAndBoughtEverything
                                        (accBoughtItems
                                            |> List.map (\bought -> { unitPrice = bought.unitPrice, count = List.length bought.ids })
                                        )

                                else
                                    RequestedAndBoughtPartially
                                        (accBoughtItems
                                            |> List.map (\bought -> { unitPrice = bought.unitPrice, count = List.length bought.ids })
                                        )
                            }
                        )
                        :: List.map (\toCmd -> toCmd modelWithItemsAndLeftoverMoneyAdded) accToCmds
                )
        in
        tryToMatch request.count [] 0 newModel []


putOffer : Offer -> ClientId -> Model -> ( Model, Cmd BackendMsg )
putOffer ({ kind, unitPrice, ids } as offer) clientId model =
    let
        playerItems : SeqDict ItemKind (List Id)
        playerItems =
            itemsForPlayer clientId model.players

        itemIds : List Id
        itemIds =
            playerItems
                |> SeqDict.get kind
                |> Maybe.withDefault []
    in
    if List.any (\id -> not (List.member id itemIds)) ids then
        -- Some of the item IDs we want to sell can't be found in our inventory!
        -- Let's bail out.
        ( model, Cmd.none )

    else
        let
            remainingItemIds : List Id
            remainingItemIds =
                -- TODO perf: sets
                itemIds
                    |> List.filter (\id -> not (List.member id ids))

            remainingPlayerItems : SeqDict ItemKind (List Id)
            remainingPlayerItems =
                if List.isEmpty remainingItemIds then
                    playerItems
                        |> SeqDict.remove kind

                else
                    playerItems
                        |> SeqDict.insert kind remainingItemIds

            updatedPlayers : Dict ClientId Player
            updatedPlayers =
                Dict.update clientId
                    (Maybe.map (\player -> { player | items = remainingPlayerItems }))
                    model.players

            newModel =
                { model | players = updatedPlayers }

            tryToMatch : List Id -> Int -> List { ids : List Id, unitPrice : Int } -> Model -> List (Model -> Cmd BackendMsg) -> ( Model, Cmd BackendMsg )
            tryToMatch restOfIds accRevenue accSoldItems accModel accToCmds =
                case SeqDict.get kind accModel.marketplace of
                    -- No requests for this item kind!
                    Nothing ->
                        finishOffer restOfIds accRevenue accSoldItems accModel accToCmds

                    Just { requests } ->
                        case MaxPriorityQueue.largest requests of
                            -- No requests for this item kind!
                            Nothing ->
                                finishOffer restOfIds accRevenue accSoldItems accModel accToCmds

                            -- We found some requests for this item kind. Let's check if we can meet any of them.
                            Just highestRequest ->
                                if unitPrice > highestRequest.unitPrice then
                                    -- I'm selling for more than the highest request is willing to buy for.
                                    -- Can't use this request, and it was our best bet - any other request
                                    -- won't be any better. End here.
                                    finishOffer restOfIds accRevenue accSoldItems accModel accToCmds

                                else
                                    -- Let's do the exchange.
                                    let
                                        itemsToSellCount : Int
                                        itemsToSellCount =
                                            -- MINI-PERF: hold that in the recursive function as an argument
                                            List.length restOfIds

                                        itemsSoldCount : Int
                                        itemsSoldCount =
                                            min itemsToSellCount highestRequest.count

                                        ( itemsSold, restOfItems ) =
                                            List.Extra.splitAt itemsSoldCount restOfIds

                                        finalPrice : Int
                                        finalPrice =
                                            -- I'm willing to sell for 200
                                            -- They're willing to pay 300
                                            -- I get 300, not 200
                                            -- From the condition above we're guaranteed this price
                                            -- is not going to be lower than our minimal price.
                                            highestRequest.unitPrice

                                        revenueAdded : Int
                                        revenueAdded =
                                            finalPrice * itemsSoldCount

                                        toBuyerCmd : Model -> Cmd BackendMsg
                                        toBuyerCmd m =
                                            Lamdera.sendToFrontend highestRequest.clientId
                                                (YouBought
                                                    { kind = kind
                                                    , count = itemsSoldCount
                                                    , unitPrice = finalPrice
                                                    , newRequests = requestsForPlayer highestRequest.clientId m.marketplace
                                                    , newItems = itemsForPlayer highestRequest.clientId m.players
                                                    , newMoney = moneyForPlayer highestRequest.clientId m.players
                                                    }
                                                )

                                        newAccModel =
                                            accModel
                                                |> removeFromHighestRequest kind itemsSoldCount
                                                |> updatePlayer highestRequest.clientId
                                                    (\player -> { player | items = addItems kind itemsSold player.items })
                                    in
                                    tryToMatch
                                        restOfItems
                                        (accRevenue + revenueAdded)
                                        ({ ids = itemsSold, unitPrice = finalPrice } :: accSoldItems)
                                        newAccModel
                                        (toBuyerCmd :: accToCmds)

            finishOffer :
                List Id
                -> Int
                -> List { ids : List Id, unitPrice : Int }
                -> Model
                -> List (Model -> Cmd BackendMsg)
                -> ( Model, Cmd BackendMsg )
            finishOffer restOfIds accRevenue accSoldItems accModel accToCmds =
                let
                    modelWithOfferAdded =
                        if restOfIds == [] then
                            -- Everything insta-sold!
                            -- We never added the offer, so we don't need to remove it.
                            -- We've removed/updated the matched requests, so we don't need to do that here.
                            accModel

                        else
                            -- Leftover request that couldn't be met by the current offers
                            let
                                finalOffer =
                                    { clientId = clientId
                                    , ids = restOfIds
                                    , unitPrice = unitPrice
                                    }
                            in
                            accModel
                                |> addOffer kind finalOffer

                    modelWithMoneyAdded =
                        modelWithOfferAdded
                            |> updatePlayer clientId
                                (\player -> { player | money = player.money + accRevenue })
                in
                ( modelWithMoneyAdded
                , Cmd.batch <|
                    Lamdera.sendToFrontend clientId
                        (YouOffered
                            { kind = kind
                            , totalCount = List.length ids
                            , unitPrice = unitPrice
                            , newOffers = offersForPlayer clientId modelWithMoneyAdded.marketplace
                            , newItems = itemsForPlayer clientId modelWithMoneyAdded.players
                            , newMoney = moneyForPlayer clientId modelWithMoneyAdded.players
                            , outcome =
                                if restOfIds == ids then
                                    OfferedButNotMet

                                else if restOfIds == [] then
                                    OfferedAndSoldEverything
                                        (accSoldItems
                                            |> List.map (\sold -> { unitPrice = sold.unitPrice, count = List.length sold.ids })
                                        )

                                else
                                    OfferedAndSoldPartially
                                        (accSoldItems
                                            |> List.map (\sold -> { unitPrice = sold.unitPrice, count = List.length sold.ids })
                                        )
                            }
                        )
                        :: List.map (\toCmd -> toCmd modelWithMoneyAdded) accToCmds
                )
        in
        tryToMatch offer.ids 0 [] newModel []


addOffer : ItemKind -> { clientId : ClientId, unitPrice : Int, ids : List Id } -> Model -> Model
addOffer kind offer model =
    { model
        | marketplace =
            model.marketplace
                |> SeqDict.update kind
                    (\maybeMarket ->
                        (case maybeMarket of
                            Nothing ->
                                { requests = MaxPriorityQueue.empty
                                , offers = MinPriorityQueue.singleton .unitPrice offer
                                }

                            Just market ->
                                { market
                                    | offers =
                                        market.offers
                                            |> MinPriorityQueue.insert .unitPrice offer
                                }
                        )
                            |> Just
                    )
    }


addRequest : ItemKind -> { clientId : ClientId, count : Int, unitPrice : Int } -> Model -> Model
addRequest kind request model =
    { model
        | marketplace =
            model.marketplace
                |> SeqDict.update kind
                    (\maybeMarket ->
                        (case maybeMarket of
                            Nothing ->
                                { requests = MaxPriorityQueue.singleton .unitPrice request
                                , offers = MinPriorityQueue.empty
                                }

                            Just market ->
                                { market
                                    | requests =
                                        market.requests
                                            |> MaxPriorityQueue.insert .unitPrice request
                                }
                        )
                            |> Just
                    )
    }


removeFromHighestRequest : ItemKind -> Int -> Model -> Model
removeFromHighestRequest kind count model =
    { model
        | marketplace =
            model.marketplace
                |> SeqDict.update kind
                    (\maybeMarket ->
                        case maybeMarket of
                            Nothing ->
                                Nothing

                            Just market ->
                                case MaxPriorityQueue.dequeue market.requests of
                                    Nothing ->
                                        Just market

                                    Just ( highestRequest, otherRequests ) ->
                                        let
                                            usedWholeRequest =
                                                count == highestRequest.count
                                        in
                                        Just
                                            { market
                                                | requests =
                                                    if usedWholeRequest then
                                                        otherRequests

                                                    else
                                                        otherRequests
                                                            |> MaxPriorityQueue.insert .unitPrice
                                                                { highestRequest | count = highestRequest.count - count }
                                            }
                    )
    }


removeFromLowestOffer : ItemKind -> List Id -> Model -> Model
removeFromLowestOffer kind ids model =
    { model
        | marketplace =
            model.marketplace
                |> SeqDict.update kind
                    (\maybeMarket ->
                        case maybeMarket of
                            Nothing ->
                                Nothing

                            Just market ->
                                case MinPriorityQueue.dequeue market.offers of
                                    Nothing ->
                                        Just market

                                    Just ( lowestOffer, otherOffers ) ->
                                        let
                                            usedWholeOffer =
                                                List.sort ids == List.sort lowestOffer.ids
                                        in
                                        Just
                                            { market
                                                | offers =
                                                    if usedWholeOffer then
                                                        otherOffers

                                                    else
                                                        otherOffers
                                                            |> MinPriorityQueue.insert .unitPrice
                                                                { lowestOffer
                                                                    | ids =
                                                                        -- TODO perf: this is O(n^2), use Set
                                                                        lowestOffer.ids
                                                                            |> List.filter (\id -> not (List.member id ids))
                                                                }
                                            }
                    )
    }


addItems : ItemKind -> List Id -> SeqDict ItemKind (List Id) -> SeqDict ItemKind (List Id)
addItems kind ids items =
    items
        |> SeqDict.update kind
            (\oldIds ->
                case oldIds of
                    Nothing ->
                        Just ids

                    Just oldIds_ ->
                        Just (oldIds_ ++ ids)
            )


offersForPlayer : ClientId -> SeqDict ItemKind ItemMarket -> List { kind : ItemKind, ids : List Id, unitPrice : Int }
offersForPlayer clientId marketplace =
    SeqDict.toList marketplace
        |> List.concatMap
            (\( kind, market ) ->
                market.offers
                    |> MinPriorityQueue.toList
                    |> List.filterMap
                        (\offer ->
                            if offer.clientId == clientId then
                                Just
                                    { kind = kind
                                    , ids = offer.ids
                                    , unitPrice = offer.unitPrice
                                    }

                            else
                                Nothing
                        )
            )


requestsForPlayer : ClientId -> SeqDict ItemKind ItemMarket -> List { kind : ItemKind, count : Int, unitPrice : Int }
requestsForPlayer clientId marketplace =
    SeqDict.toList marketplace
        |> List.concatMap
            (\( kind, market ) ->
                market.requests
                    |> MaxPriorityQueue.toList
                    |> List.filterMap
                        (\request ->
                            if request.clientId == clientId then
                                Just
                                    { kind = kind
                                    , count = request.count
                                    , unitPrice = request.unitPrice
                                    }

                            else
                                Nothing
                        )
            )


moneyForPlayer : ClientId -> Dict ClientId Player -> Int
moneyForPlayer clientId players =
    Dict.get clientId players
        |> Maybe.map .money
        |> Maybe.withDefault 0


itemsForPlayer : ClientId -> Dict ClientId Player -> SeqDict ItemKind (List Id)
itemsForPlayer clientId players =
    Dict.get clientId players
        |> Maybe.map .items
        |> Maybe.withDefault SeqDict.empty


updatePlayer : ClientId -> (Player -> Player) -> Model -> Model
updatePlayer clientId fn model =
    { model | players = Dict.update clientId (Maybe.map fn) model.players }
