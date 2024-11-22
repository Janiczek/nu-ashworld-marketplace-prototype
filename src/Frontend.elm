module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Html.Extra as HH
import Lamdera
import List.Cartesian
import List.Extra
import SeqDict
import Time
import Types exposing (..)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = \_ _ -> init
        , onUrlRequest = \_ -> NoOpFrontendMsg
        , onUrlChange = \_ -> NoOpFrontendMsg
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd FrontendMsg )
init =
    ( { myOffers = []
      , myRequests = []
      , myMoney = 0
      , myItems = SeqDict.empty
      , message = ""
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        ClearMessage ->
            ( { model | message = "" }
            , Cmd.none
            )

        PutOfferClicked { kind, unitPrice, ids } ->
            ( model
            , Lamdera.sendToBackend
                (PutOffer
                    { kind = kind
                    , unitPrice = unitPrice
                    , ids = ids
                    }
                )
            )

        PutRequestClicked request ->
            ( model
            , Lamdera.sendToBackend (PutRequest request)
            )

        CancelOfferClicked offer ->
            ( model
            , Lamdera.sendToBackend (CancelOffer offer)
            )

        CancelRequestClicked request ->
            ( model
            , Lamdera.sendToBackend (CancelRequest request)
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        YourBelongings belongings ->
            ( { model
                | myOffers = belongings.offers
                , myRequests = belongings.requests
                , myItems = belongings.items
                , myMoney = belongings.money
              }
            , Cmd.none
            )

        YouOffered youOffered ->
            ( { model
                | myOffers = youOffered.newOffers
                , myItems = youOffered.newItems
                , message =
                    case youOffered.outcome of
                        OfferedAndSoldEverything sold ->
                            let
                                totalItems : Int
                                totalItems =
                                    List.sum (List.map .count sold)

                                totalRevenue : Int
                                totalRevenue =
                                    List.sum (List.map (\{ unitPrice, count } -> unitPrice * count) sold)

                                averageUnitPrice : Int
                                averageUnitPrice =
                                    totalRevenue // totalItems
                            in
                            "You sold: {COUNT}x {KIND} for an average of {AVERAGEUNITPRICE} each = total {TOTALREVENUE}. The money has been added to your account. You now have {MONEY} money."
                                |> String.replace "{COUNT}" (String.fromInt totalItems)
                                |> String.replace "{KIND}" (Debug.toString youOffered.kind)
                                |> String.replace "{AVERAGEUNITPRICE}" (String.fromInt averageUnitPrice)
                                |> String.replace "{TOTALREVENUE}" (String.fromInt totalRevenue)
                                |> String.replace "{MONEY}" (String.fromInt youOffered.newMoney)

                        OfferedAndSoldPartially sold ->
                            let
                                totalItemsSold : Int
                                totalItemsSold =
                                    List.sum (List.map .count sold)

                                totalItemsLeft : Int
                                totalItemsLeft =
                                    youOffered.totalCount - totalItemsSold

                                totalRevenue : Int
                                totalRevenue =
                                    List.sum (List.map (\{ unitPrice, count } -> unitPrice * count) sold)

                                averageUnitPrice : Int
                                averageUnitPrice =
                                    totalRevenue // totalItemsSold

                                minimumUnitPrice : Int
                                minimumUnitPrice =
                                    List.minimum (List.map .unitPrice sold)
                                        |> Maybe.withDefault 0
                            in
                            "You sold: {COUNTSOLD}x {KIND} for an average of {AVERAGEUNITPRICE} each = total {TOTALREVENUE}. You have {COUNTLEFT}x {KIND} left to sell, with a minimum price of {MINIMUMUNITPRICE}. These items are left in the marketplace and are waiting for a buyer. The gained money has been added to your account. You now have {MONEY} money."
                                |> String.replace "{COUNTSOLD}" (String.fromInt totalItemsSold)
                                |> String.replace "{KIND}" (Debug.toString youOffered.kind)
                                |> String.replace "{AVERAGEUNITPRICE}" (String.fromInt averageUnitPrice)
                                |> String.replace "{TOTALREVENUE}" (String.fromInt totalRevenue)
                                |> String.replace "{COUNTLEFT}" (String.fromInt totalItemsLeft)
                                |> String.replace "{MINIMUMUNITPRICE}" (String.fromInt minimumUnitPrice)
                                |> String.replace "{MONEY}" (String.fromInt youOffered.newMoney)

                        OfferedButNotMet ->
                            "You offered: {COUNT}x {KIND} but none were sold. The items remain in the marketplace and wait for a buyer."
                                |> String.replace "{COUNT}" (String.fromInt youOffered.totalCount)
                                |> String.replace "{KIND}" (Debug.toString youOffered.kind)
              }
            , Cmd.none
            )

        YouCanceledOffer { kind, count, unitPrice, newOffers, newItems } ->
            ( { model
                | myOffers = newOffers
                , myItems = newItems
                , message =
                    "You canceled offer: {COUNT}x {KIND} for {UNITPRICE} each = total {TOTALPRICE}. The items have been put back into your inventory."
                        |> String.replace "{COUNT}" (String.fromInt count)
                        |> String.replace "{KIND}" (Debug.toString kind)
                        |> String.replace "{UNITPRICE}" (String.fromInt unitPrice)
                        |> String.replace "{TOTALPRICE}" (String.fromInt (count * unitPrice))
              }
            , Cmd.none
            )

        YouSold youSold ->
            ( { model
                | myOffers = youSold.newOffers
                , myMoney = youSold.newMoney
                , message =
                    "You sold: {COUNT}x {KIND} for {UNITPRICE} each = total {TOTALREVENUE}. The gained money has been added to your account. You now have {MONEY} money."
                        |> String.replace "{COUNT}" (String.fromInt youSold.count)
                        |> String.replace "{KIND}" (Debug.toString youSold.kind)
                        |> String.replace "{UNITPRICE}" (String.fromInt youSold.unitPrice)
                        |> String.replace "{TOTALREVENUE}" (String.fromInt (youSold.count * youSold.unitPrice))
                        |> String.replace "{MONEY}" (String.fromInt youSold.newMoney)
              }
            , Cmd.none
            )

        YouRequested { kind, totalCount, unitPrice, newRequests, newMoney, outcome } ->
            let
                totalPrice : Int
                totalPrice =
                    totalCount * unitPrice
            in
            ( { model
                | myRequests = newRequests
                , myMoney = newMoney
                , message =
                    case outcome of
                        RequestedButNotMet ->
                            "You requested: {COUNT}x {KIND} for {UNITPRICE} each = total {TOTALPRICE}. The money has been reserved from your account."
                                |> String.replace "{COUNT}" (String.fromInt totalCount)
                                |> String.replace "{KIND}" (Debug.toString kind)
                                |> String.replace "{UNITPRICE}" (String.fromInt unitPrice)
                                |> String.replace "{TOTALPRICE}" (String.fromInt totalPrice)

                        RequestedAndBoughtPartially bought ->
                            let
                                totalItemsBought : Int
                                totalItemsBought =
                                    List.sum (List.map .count bought)

                                totalItemsLeft : Int
                                totalItemsLeft =
                                    totalCount - totalItemsBought

                                totalRealPrice : Int
                                totalRealPrice =
                                    List.sum (List.map (\x -> x.count * x.unitPrice) bought)
                            in
                            "You requested: {COUNT}x {KIND} and were able to immediately buy {TOTALITEMSBOUGHT}x {KIND} for an average of {TOTALREALPRICE}. You have {COUNTLEFT}x {KIND} left to buy, with a maximum price of {UNITPRICE}. Your money has been reserved from your account."
                                |> String.replace "{COUNT}" (String.fromInt totalCount)
                                |> String.replace "{KIND}" (Debug.toString kind)
                                |> String.replace "{UNITPRICE}" (String.fromInt unitPrice)
                                |> String.replace "{TOTALITEMSBOUGHT}" (String.fromInt totalItemsBought)
                                |> String.replace "{TOTALREALPRICE}" (String.fromInt totalRealPrice)
                                |> String.replace "{COUNTLEFT}" (String.fromInt totalItemsLeft)

                        RequestedAndBoughtEverything bought ->
                            let
                                totalItemsBought : Int
                                totalItemsBought =
                                    List.sum (List.map .count bought)

                                totalRealPrice : Int
                                totalRealPrice =
                                    List.sum (List.map (\x -> x.count * x.unitPrice) bought)
                            in
                            "You requested and bought {COUNT}x {KIND} for an average price of {AVERAGEUNITPRICE} each = total {TOTALREALPRICE}. The items have been added to your account."
                                |> String.replace "{COUNT}" (String.fromInt totalCount)
                                |> String.replace "{KIND}" (Debug.toString kind)
                                |> String.replace "{TOTALREALPRICE}" (String.fromInt totalRealPrice)
                                |> String.replace "{AVERAGEUNITPRICE}" (String.fromInt (totalPrice // totalItemsBought))
              }
            , Cmd.none
            )

        YouCanceledRequest { kind, count, unitPrice, newRequests, newMoney } ->
            ( { model
                | myRequests = newRequests
                , myMoney = newMoney
                , message =
                    "You canceled request: {COUNT}x {KIND} for {UNITPRICE} each = total {TOTALPRICE}. The reserved money has been returned to your account."
                        |> String.replace "{COUNT}" (String.fromInt count)
                        |> String.replace "{KIND}" (Debug.toString kind)
                        |> String.replace "{UNITPRICE}" (String.fromInt unitPrice)
                        |> String.replace "{TOTALPRICE}" (String.fromInt (count * unitPrice))
              }
            , Cmd.none
            )

        YouBought { kind, count, unitPrice, newRequests, newItems, newMoney } ->
            ( { model
                | myRequests = newRequests
                , myItems = newItems
                , message =
                    "You bought: {COUNT}x {KIND} for {UNITPRICE} each = total {TOTALPRICE}. The items have been added to your inventory.{MONEY}"
                        |> String.replace "{COUNT}" (String.fromInt count)
                        |> String.replace "{KIND}" (Debug.toString kind)
                        |> String.replace "{UNITPRICE}" (String.fromInt unitPrice)
                        |> String.replace "{TOTALPRICE}" (String.fromInt (count * unitPrice))
                        |> String.replace "{MONEY}"
                            (if model.myMoney == newMoney then
                                ""

                             else
                                " You bought some of these under price, netting you {DIFF} money. You now have {FINAL} money."
                                    |> String.replace "{DIFF}" (String.fromInt (newMoney - model.myMoney))
                                    |> String.replace "{FINAL}" (String.fromInt newMoney)
                            )
              }
            , Cmd.none
            )


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    if String.isEmpty model.message then
        Sub.none

    else
        Time.every 5000 (\_ -> ClearMessage)


title : String
title =
    "NuAshworld - Marketplace prototype"


view : Model -> Browser.Document FrontendMsg
view model =
    { title = title
    , body =
        [ H.h1 [] [ H.text title ]
        , H.div [] [ H.text <| "My Money: " ++ String.fromInt model.myMoney ]
        , if String.isEmpty model.message then
            H.text ""

          else
            H.div [] [ H.text <| "Message: " ++ model.message ]
        , H.div []
            [ H.h2 [] [ H.text "My Items" ]
            , H.ul []
                (model.myItems
                    |> SeqDict.toList
                    |> List.sortBy (Tuple.first >> Debug.toString)
                    |> List.map
                        (\( kind, ids ) ->
                            let
                                count =
                                    List.length ids

                                offerButton : Int -> Int -> H.Html FrontendMsg
                                offerButton offerCount price =
                                    H.button
                                        [ HE.onClick
                                            (PutOfferClicked
                                                { kind = kind
                                                , unitPrice = price
                                                , ids = List.take offerCount ids
                                                }
                                            )
                                        ]
                                        [ "Offer {COUNT}x, {PRICE}ea"
                                            |> String.replace "{COUNT}" (String.fromInt offerCount)
                                            |> String.replace "{PRICE}" (String.fromInt price)
                                            |> H.text
                                        ]
                            in
                            H.li
                                [ HA.style "display" "flex"
                                , HA.style "gap" "4px"
                                ]
                                [ "{COUNT}x {KIND} ({IDS})"
                                    |> String.replace "{COUNT}" (String.fromInt count)
                                    |> String.replace "{KIND}" (Debug.toString kind)
                                    |> String.replace "{IDS}"
                                        (ids
                                            |> List.map String.fromInt
                                            |> String.join ", "
                                        )
                                    |> H.text
                                , offerButton 1 100
                                , offerButton count 100
                                    |> HH.viewIf (count > 1)
                                , offerButton 1 200
                                , offerButton count 200
                                    |> HH.viewIf (count > 1)
                                ]
                        )
                )
            ]
        , H.div []
            [ H.h2 [] [ H.text "My Offers" ]
            , H.text <| Debug.toString model.myOffers
            ]
        , H.div []
            [ H.h2 [] [ H.text "My Requests" ]
            , H.text <| Debug.toString model.myRequests
            , H.div []
                [ H.h3 [] [ H.text "Make a Request" ]
                , H.div []
                    (List.Cartesian.map3
                        (\kind price count ->
                            H.button
                                [ HE.onClick
                                    (PutRequestClicked
                                        { kind = kind
                                        , count = count
                                        , unitPrice = price
                                        }
                                    )
                                ]
                                [ H.text <|
                                    String.fromInt count
                                        ++ "x "
                                        ++ Debug.toString kind
                                        ++ " "
                                        ++ String.fromInt price
                                        ++ "ea"
                                ]
                        )
                        [ Gun, Ammo, Armor, Food ]
                        [ 50, 100, 200, 300 ]
                        [ 1, 3 ]
                    )
                ]
            ]
        ]
    }
