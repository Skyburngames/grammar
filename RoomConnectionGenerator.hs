module RoomConnectionGenerator
(
  connectRoomHorizontal,
  connectRoomVertical
) where

import Grammar

connectRoomHorizontal::Room->Room->RoomConnector
connectRoomHorizontal room1 room2 = RoomConnector (roomId room1) (roomId room2) (Position 5 2) (Position 0 2) Horizontal

connectRoomVertical::Room->Room->RoomConnector
connectRoomVertical room1 room2 = RoomConnector (roomId room1) (roomId room2) (Position 2 (-5)) (Position 2 0) Vertical
