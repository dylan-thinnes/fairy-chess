module Glade.UI where

import Prelude
import Data.Maybe
import Data.Array
import Data.Const

import Glade.Boards
import Glade.Pieces

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data TileState name team
    = TileState { pieces :: Array (Piece name team)
                , mode :: Maybe TileMode
                }

data TileMode = UnderThreat | CanMove

data TileQuery name team a
    = SetPiece (Array (Piece name team) -> Array (Piece name team)) a
    | SetMode (Maybe TileMode -> Maybe TileMode)

data TileMessage
    = Selected
    | Deselected

type Tile name team m = H.Component HH.HTML (TileQuery name team) (Array (Piece name team)) TileMessage m

basicTile :: forall slot name team location m. 
          (UI slot name team location)
          => slot -> Array HH.ClassName -> Tile name team m
basicTile slot additionalClasses =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }

    where

    initialState :: Array (Piece name team) -> (TileState name team)
    initialState pieces = TileState { pieces: pieces, mode: Nothing }

    render :: (TileState name team) -> H.ComponentHTML Unit () m
    render (TileState { pieces }) 
        = HH.div [ HP.classes $ [HH.ClassName "tile"] <> additionalClasses ]
                 (fromFoldable piece)
        where
            piece = head pieces 
                    <#> \p -> HH.img [HP.src $ pieceUrl slot p, HP.draggable false]

    handleAction :: Unit -> H.HalogenM (TileState name team) Unit () TileMessage m Unit
    handleAction _ = pure unit

type Board m = H.Component HH.HTML (Const Unit) Unit Void m
data UIState name team location
  = AwaitingInput
  | TileSelected (Maybe location)
  | Promotion (Maybe (Piece name team)) (Maybe (Array (Piece name team)))
  | AlertingThreat (Maybe (Piece name team)) (Maybe location)

basicBoard :: forall slot name team location m. 
           (UI slot name team location)
           => slot -> Board m
basicBoard slot =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }

    where

    initialState :: Unit -> UIState name team location
    initialState _ = AwaitingInput

    render :: UIState name team location -> H.ComponentHTML Unit () m
    render _ = HH.div [HP.class_ $ H.ClassName "board"] []

    handleAction :: Unit -> H.HalogenM (UIState name team location) Unit () Void m Unit
    handleAction _ = pure unit

class ( IsBoard slot location, IsPiece slot name team location )
     <= UI slot name team location | slot -> name team location where
    pieceUrl :: slot -> Piece name team -> String
    tile :: forall m. slot -> location -> Tile name team m
    board :: forall m. slot -> Board m
