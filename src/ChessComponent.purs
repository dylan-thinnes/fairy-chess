module ChessComponent where

import Chess

import Prelude
import Data.Array
import Data.Maybe
import Data.Foldable (and)
import Data.Set as S

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.HTML.Elements (Leaf)
import DOM.HTML.Indexed (HTMLimg)

import Data.Tuple
import Data.String.CodeUnits (singleton)
import Data.String.Unsafe (charAt)

import Control.MonadZero

-- component
type State = { board :: Board, active :: Maybe XY, movedPieces :: S.Set Piece }
data Action = Select XY | Deselect

editBoard :: (Board -> Board) -> State -> State
editBoard f state = state { board = f state.board }

pieceToImg = pieceToUrl >>> urlToImg
urlToImg url = HH.img [HP.src url, HP.draggable false]
pieceToUrl (Piece team pieceType) = "./pieces/Chess_" <> p <> c <> "t45.svg"
    where
    c = case team of
          Black -> "d"
          White -> "l"
    p = case pieceType of
          Rook1   -> "r"
          Rook2   -> "r"
          Knight1 -> "n"
          Knight2 -> "n"
          Bishop1 -> "b"
          Bishop2 -> "b"
          Queen   -> "q"
          King    -> "k"
          _       -> "p"


component :: forall q o m. H.Component HH.HTML q Unit o m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
    where

    initialState :: Unit -> State
    initialState _ = { board: startingBoard, active: Nothing, movedPieces: S.empty }

    render :: State -> H.ComponentHTML Action () m
    render state = HH.div [HP.class_ $ HH.ClassName "board-container"] [htmlBoard]
        where
        -- dimensions
        boardHeight = length state.board
        boardWidth  = maybe 0 length $ state.board !! 0

        -- IDs for each row / column
        rowIds    = guard (boardHeight > 0) *> ((boardHeight-1)..0)
        columnIds = guard (boardWidth > 0)  *> (0..(boardWidth-1))

        -- Generate a rowEdge from an id, convert id to letter
        rowEdgePiece id = HH.div [HP.class_ $ HH.ClassName "left-edge-piece"] 
                                  [HH.text $ toName id]
            where
            toName ii = (if ii > 25 then toName (ii / 26) else "") <> toAlphabet (ii `mod` 26)
            toAlphabet i = singleton $ charAt i "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        -- Generate a columnEdge from an id, increment id to natural number
        columnEdgePiece id = HH.div [HP.class_ $ HH.ClassName "bottom-edge-piece"] 
                                    [HH.text $ show $ id + 1]

        -- Generate a row from an id & array of tiles
        generateRow rowId tiles = HH.div [HP.class_ $ HH.ClassName "chess-row"]
                                       $ [rowEdgePiece rowId] <> tileRow <> [rowEdgePiece rowId]
            where
            tileRow = zip columnIds tiles <#> \(Tuple columnId tile) 
                                                -> generateTile tile { x: columnId, y: rowId }

        -- Static corners & bottom edges
        corner               = HH.div [HP.class_ $ HH.ClassName "corner"] []
        bottomEdge           = HH.div [HP.class_ $ HH.ClassName "bottom-edge"]
                                    $ [corner] <> map columnEdgePiece columnIds <> [corner]

        -- Full board
        htmlBoard = HH.div [HP.class_ $ HH.ClassName "chess-board"]
                         $ [bottomEdge] <> mainRows <> [bottomEdge]
        mainRows = map (uncurry generateRow) $ zip rowIds state.board

        -- Tiles
        generateTile tile position = HH.div properties pieceImg
            where
            isActive = state.active == Just position
            properties = [ HP.classes $ map HH.ClassName 
                                      $ ["tile"] <> if isActive then ["selected"] else []
                         , HE.onClick $ \_ -> if isActive then Just Deselect else Just $ Select position
                         ]
            pieceImg   = case tile of
                           Just piece -> [pieceToImg piece]
                           Nothing    -> []

    handleAction :: Action -> H.HalogenM State Action () o m Unit
    handleAction (Select newPosition) = do
        existingPosition <- H.gets \state -> state.active
        case existingPosition of
            Nothing -> setNewPosition newPosition
            Just oldPosition -> tryMove oldPosition newPosition

        where
        -- Set a new position
        setNewPosition position = H.modify_ $ \state -> state { active = Just position }

        -- Do some precondition checks before moving
        tryMove oldPosition newPosition = do
            board <- H.gets \state -> state.board
            let oldTile = join $ getCoord oldPosition board
            let passes = and
                            [ oldTile /= Nothing
                            , true -- newPosition `S.member` canMove board oldPosition
                            ]
            if passes then pure unit else do
                H.modify_ $ \state -> state { board = setCoord oldPosition Nothing state.board }
                H.modify_ $ \state -> state { board = setCoord newPosition oldTile state.board }
                H.modify_ $ \state -> state { active = Nothing }

    handleAction Deselect = H.modify_ $ \state -> state { active = Nothing }


