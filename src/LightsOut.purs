module LightsOut (component, XY, neighbours) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple
import Data.Array (concat, index, (!!), replicate, updateAt, zip, range, (..))
import Control.MonadZero (guard)
import Data.Traversable (sequence)
import Data.Ord (abs)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Data.Lens (Lens, Lens', Traversal', set, view, over, preview)
import Data.Lens.Index (ix)

type Board = Array Row
type Row   = Array Cell
type Cell  = Boolean
type XY = { x :: Int, y :: Int }
data Action = Toggle XY | Reset XY
type State = { board :: Board, dimensions :: XY }

newBoard :: XY -> Board
newBoard dimensions = replicate dimensions.y $ replicate dimensions.x false

toLens :: XY -> Traversal' Board Boolean
toLens { x, y } = ix y <<< ix x

getCoord  coord = preview (toLens coord)
setCoord  coord = set     (toLens coord)
editCoord coord = over    (toLens coord)

editBoard :: (Board -> Board) -> State -> State
editBoard f state = state { board = f state.board }

neighbours :: XY -> XY -> Array XY
neighbours coord@({ x, y }) bounds@({ x: maxX, y: maxY })
    = do
        cx <- (x-1)..(x+1)
        cy <- (y-1)..(y+1)
        guard $ cx >= 0
        guard $ cy >= 0
        guard $ cx < maxX
        guard $ cy < maxY
        guard $ abs (x - cx) + abs (y - cy) < 2
        pure { x: cx, y: cy }

component :: forall q o m. H.Component HH.HTML q XY o m
component = 
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval 
            { handleAction = handleAction 
            , receive = Just <<< Reset
            }
        }

    where

    initialState :: XY -> State
    initialState dimensions = { board: newBoard dimensions, dimensions }

    render :: State -> H.ComponentHTML Action () m
    render { dimensions, board }
      = HH.div [HP.class_ $ HH.ClassName "game"] 
      $ map (newRow dimensions.x) (zip board $ 0..(dimensions.y-1))

    newRow :: Int -> Tuple Row Int -> H.ComponentHTML Action () m
    newRow maxWidth (Tuple row y)
      = HH.div [HP.class_ $ HH.ClassName "row"] 
      $ zip row (0..(maxWidth-1))
        <#> \(Tuple cell x) -> newLight { x, y } cell

    newLight :: XY -> Cell -> H.ComponentHTML Action () m
    newLight coord state = HH.div
        [ HE.onClick $ \_ -> Just $ Toggle coord
        , HP.classes
            [ HH.ClassName "light"
            , HH.ClassName $ if state then "on" else "off"
            ]
        ] []

    handleAction :: Action -> H.HalogenM State Action () o m Unit
    handleAction (Toggle coord)
      = do
          bounds <- H.gets \state -> state.dimensions
          _ <- sequence $ 
                    (neighbours coord bounds) <#>
                    \c -> H.modify_ $ editBoard $ editCoord c not
          pure unit
    handleAction (Reset dimensions)
      = H.put $ initialState dimensions
