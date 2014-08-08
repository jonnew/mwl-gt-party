module Types where

------------------------------------------------------------------------------
import qualified Data.Map       as Map
import           Graphics.Gloss

------------------------------------------------------------------------------ 
data World = World {
    width          :: Float
  , height         :: Float
  , resources      :: Map.Map String Picture
  , gameState      :: GameState
  }

data GameState = WaitingBoth
               | ConfirmReset GameState
               | RequestButtonsUp
               | GotOne  Player Choice
               | GotBoth Choice Choice
               deriving (Eq)

data Player = PlayerLeft
            | PlayerRight
            deriving (Eq)

data Choice = Cooperate
            | Cheat
            deriving (Eq)
