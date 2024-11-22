module Types exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import MaxPriorityQueue exposing (MaxPriorityQueue)
import MinPriorityQueue exposing (MinPriorityQueue)
import SeqDict exposing (SeqDict)


type alias Offer =
    { kind : ItemKind, unitPrice : Int, ids : List Id }


type alias Request =
    { kind : ItemKind, unitPrice : Int, count : Int }


type alias FrontendModel =
    { myOffers : List Offer
    , myRequests : List Request
    , myItems : SeqDict ItemKind (List Id)
    , myMoney : Int
    , message : String
    }


type alias Id =
    Int


type ItemKind
    = Gun
    | Ammo
    | Armor
    | Food


type alias Player =
    { money : Int
    , items : SeqDict ItemKind (List Id)
    }


type alias ItemMarket =
    { offers : MinPriorityQueue { clientId : ClientId, ids : List Id, unitPrice : Int }
    , requests : MaxPriorityQueue { clientId : ClientId, count : Int, unitPrice : Int } -- implicitly also holds money from the requesters
    }


type alias BackendModel =
    { marketplace : SeqDict ItemKind ItemMarket
    , players : Dict ClientId Player
    , nextId : Id
    }


type FrontendMsg
    = NoOpFrontendMsg
    | PutOfferClicked Offer
    | PutRequestClicked Request
    | CancelOfferClicked Offer
    | CancelRequestClicked Request
    | ClearMessage


type ToBackend
    = PutOffer Offer
    | PutRequest Request
    | CancelOffer Offer
    | CancelRequest Request


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type OfferOutcome
    = OfferedButNotMet
    | OfferedAndSoldPartially (List { unitPrice : Int, count : Int })
    | OfferedAndSoldEverything (List { unitPrice : Int, count : Int })


type RequestOutcome
    = RequestedButNotMet
    | RequestedAndBoughtPartially (List { unitPrice : Int, count : Int })
    | RequestedAndBoughtEverything (List { unitPrice : Int, count : Int })


type ToFrontend
    = YourBelongings
        { offers : List Offer
        , requests : List Request
        , items : SeqDict ItemKind (List Id)
        , money : Int
        }
      -- "My Offers" side
    | YouOffered
        { kind : ItemKind
        , totalCount : Int
        , unitPrice : Int
        , newOffers : List Offer
        , newItems : SeqDict ItemKind (List Id)
        , newMoney : Int
        , outcome : OfferOutcome
        }
    | YouCanceledOffer
        { kind : ItemKind
        , count : Int
        , unitPrice : Int
        , newOffers : List Offer
        , newItems : SeqDict ItemKind (List Id)
        }
    | YouSold
        { kind : ItemKind
        , count : Int
        , unitPrice : Int
        , newOffers : List Offer
        , newMoney : Int
        }
      -- "My Requests" side
    | YouRequested
        { kind : ItemKind
        , totalCount : Int
        , unitPrice : Int
        , newRequests : List Request
        , newMoney : Int
        , newItems : SeqDict ItemKind (List Id)
        , outcome : RequestOutcome
        }
    | YouCanceledRequest
        { kind : ItemKind
        , count : Int
        , unitPrice : Int
        , newRequests : List Request
        , newMoney : Int
        }
    | YouBought
        { kind : ItemKind
        , count : Int
        , unitPrice : Int
        , newRequests : List Request
        , newItems : SeqDict ItemKind (List Id)
        , newMoney : Int
        }
