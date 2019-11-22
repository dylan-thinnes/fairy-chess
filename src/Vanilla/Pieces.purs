module Vanilla.Pieces where

import Movement
import Prelude
import Data.Tuple
import Data.Maybe
import Data.Array as Array
import Data.Array ((!!))
import Data.List
import Type.Proxy
import Control.Monad.Loops
import Control.MonadZero (guard)

data Name = Pawn | Rook | Knight | Bishop | King | Queen

isPawn :: Name -> Boolean
isPawn = case _ of
            Pawn -> true
            _    -> false

data Team = Black | White
derive instance eqTeam :: Eq Team

teamDirection White = 1
teamDirection Black = -1

data Vanilla = Vanilla

instance vanillaPiece :: IsPiece Vanilla Name Team D2 where
    initialState _
        = [ Tuple (Piece Pawn   White 1) $ D2 { x: 1, y: 2 }
          , Tuple (Piece Pawn   White 2) $ D2 { x: 2, y: 2 }
          , Tuple (Piece Pawn   White 3) $ D2 { x: 3, y: 2 }
          , Tuple (Piece Pawn   White 4) $ D2 { x: 4, y: 2 }
          , Tuple (Piece Pawn   White 5) $ D2 { x: 5, y: 2 }
          , Tuple (Piece Pawn   White 6) $ D2 { x: 6, y: 2 }
          , Tuple (Piece Pawn   White 7) $ D2 { x: 7, y: 2 }
          , Tuple (Piece Pawn   White 8) $ D2 { x: 8, y: 2 }

          , Tuple (Piece Rook   White 1) $ D2 { x: 1, y: 1 }
          , Tuple (Piece Knight White 1) $ D2 { x: 2, y: 1 }
          , Tuple (Piece Bishop White 1) $ D2 { x: 3, y: 1 }
          , Tuple (Piece King   White 1) $ D2 { x: 4, y: 1 }
          , Tuple (Piece Queen  White 1) $ D2 { x: 5, y: 1 }
          , Tuple (Piece Bishop White 2) $ D2 { x: 6, y: 1 }
          , Tuple (Piece Knight White 2) $ D2 { x: 7, y: 1 }
          , Tuple (Piece Rook   White 2) $ D2 { x: 8, y: 1 }

          , Tuple (Piece Pawn   Black 1) $ D2 { x: 1, y: 7 }
          , Tuple (Piece Pawn   Black 2) $ D2 { x: 2, y: 7 }
          , Tuple (Piece Pawn   Black 3) $ D2 { x: 3, y: 7 }
          , Tuple (Piece Pawn   Black 4) $ D2 { x: 4, y: 7 }
          , Tuple (Piece Pawn   Black 5) $ D2 { x: 5, y: 7 }
          , Tuple (Piece Pawn   Black 6) $ D2 { x: 6, y: 7 }
          , Tuple (Piece Pawn   Black 7) $ D2 { x: 7, y: 7 }
          , Tuple (Piece Pawn   Black 8) $ D2 { x: 8, y: 7 }

          , Tuple (Piece Rook   Black 1) $ D2 { x: 1, y: 8 }
          , Tuple (Piece Knight Black 1) $ D2 { x: 2, y: 8 }
          , Tuple (Piece Bishop Black 1) $ D2 { x: 3, y: 8 }
          , Tuple (Piece King   Black 1) $ D2 { x: 4, y: 8 }
          , Tuple (Piece Queen  Black 1) $ D2 { x: 5, y: 8 }
          , Tuple (Piece Bishop Black 2) $ D2 { x: 6, y: 8 }
          , Tuple (Piece Knight Black 2) $ D2 { x: 7, y: 8 }
          , Tuple (Piece Rook   Black 2) $ D2 { x: 8, y: 8 }
          ]

    toMoveSpec _ self@(Piece name team i)
        = if isPawn name
          then Sequence
                 (Choice
                   [ Require -- One step forward
                       (isNothing <$> occupiedBy (Relative $ D2 { x: 0, y: teamDirection team * 1 }))
                       $ Finally [Move self $ Relative $ D2 { x: 0, y: teamDirection team * 1 }]
                                 (pure true)
                   , Require -- Two steps forward
                       ( do
                           between <- occupiedBy (Relative $ D2 { x: 0, y: teamDirection team * 1 })
                           occupant <- occupiedBy (Relative $ D2 { x: 0, y: teamDirection team * 2 })
                           notMoved <- neverMoved self
                           pure $ between == Nothing && occupant == Nothing && notMoved
                       )
                       $ Finally [Move self $ Relative $ D2 { x: 0, y: teamDirection team * 2 }]
                                 (pure true)
                   , Require -- Positive x diagonal capture
                       ( do
                           mp <- occupiedBy (Relative $ D2 { x: 1, y: teamDirection team * 1 })
                           case mp of
                                Just (Piece _ t _) -> pure $ t /= team
                                _                  -> pure false
                       )
                       $ Finally [Clear $ Relative $ D2 { x: 1, y: teamDirection team * 1 }
                                 ,Move self $ Relative $ D2 { x: 1, y: teamDirection team * 1 }
                                 ]
                                 (pure true)
                   , Require -- Negative x diagonal capture
                       ( do
                           (Piece _ t _) <- try =<< occupiedBy (Relative $ D2 { x: -1, y: teamDirection team * 1 })
                           pure $ t /= team
                       )
                       $ Finally [Clear $ Relative $ D2 { x: -1, y: teamDirection team * 1 }
                                 ,Move self $ Relative $ D2 { x: -1, y: teamDirection team * 1 }
                                 ]
                                 (pure true)
                   , Require -- Positive x en passant capture
                       ( do
                           -- Check there is an immediately adjacent pawn of the opposing team
                           p@(Piece n t _) <- try =<< occupiedBy (Relative $ D2 { x: 1, y: 0 })
                           guard $ isPawn n && t /= team

                           totalHist <- movementHistoryComplete
                           pieceHist <- movementHistoryPiece p

                           -- Check that the pawn has only ever made one action
                           guard $ Array.length pieceHist == 2
                           (MovementEvent p0@(D2 { x: x0, y: y0 }) _ _) <- try $ Array.index pieceHist 0
                           (MovementEvent p1@(D2 { x: x1, y: y1 }) _ i) <- try $ Array.index pieceHist 1

                           -- Check that the opposing pawn meets other criteria
                           -- for being en-passant-capturable
                           pure $ x0 == x1 -- Is the opposing pawn on the same column?
                               && y1 == (y0 + teamDirection t * 2) -- Did the opposing pawn double jump from its starting position?
                               && i == Array.length totalHist -- Is the last movement event of the opposing pawn also the last movement in the game overall
                       )
                       $ Finally [Clear $ Relative $ D2 { x: 1, y: 0 }
                                 ,Move self $ Relative $ D2 { x: 1, y: teamDirection team * 1 }
                                 ]
                                 (pure true)
                   ])
                 (Require
                    ( do
                        (D2 { x, y }) <- atPiece self
                        pure $ x == if team == White then 8 else 1
                    )
                    $ Finally [Promote self $ map (\t -> Piece t team (-1)) [Queen, Rook, Bishop, Knight]]
                              (pure true)
                 )
          else Finally [] (pure true)
        {- = if isPawn name
             then Sequence 
                    (Choice
                      [ Require [Unoccupied $ Relative $ D2 { x: 0, y: teamDirection team * 1 }] 
                          $ Finally [Move self $ Relative $ D2 { x: 0, y: teamDirection team * 1 }] []
                      , Require [Unoccupied $ Relative $ D2 { x: 0, y: teamDirection team * 2 }
                                ,NeverMoved self
                                ]
                          $ Finally [Move self $ Relative $ D2 { x: 0, y: teamDirection team * 2 }] []
                      , Require [OccupiedBy (Relative $ D2 { x: 1, y: teamDirection team * 1 }) 
                                            (\(Piece _ t _) -> t /= team)
                                ]
                          $ Finally [Move self $ Relative $ D2 { x: 1, y: teamDirection team * 1 }] []
                      , Require [OccupiedBy (Relative $ D2 { x: -1, y: teamDirection team * 1 }) 
                                            (\(Piece _ t _) -> t /= team)
                                ]
                          $ Finally [Move self $ Relative $ D2 { x: -1, y: teamDirection team * 1 }] []
                      , Require [OccupiedBy (Relative $ D2 { x: -1, y: 0 }) 
                                            (\(Piece n t _) -> t /= team && isPawn n)
                                ]
                          $ Finally [Move self $ Relative $ D2 { x: -1, y: teamDirection team * 1 }] []
                      , Require [OccupiedBy (Relative $ D2 { x: 1, y: 0 }) 
                                            (\(Piece n t _) -> t /= team && isPawn n)
                                ]
                          $ Finally [Move self $ Relative $ D2 { x: 1, y: teamDirection team * 1 }] []
                      ])
                    (Require [AtPiece self (\(D2 { x, y }) -> x == if team == White then 8 else 1)] 
                      $ Finally [Promote self $ map (\t -> Piece t team (-1)) [Queen, Rook, Bishop, Knight]]
                                []
                    )
             else case name of _ -> Finally [] [] -}
