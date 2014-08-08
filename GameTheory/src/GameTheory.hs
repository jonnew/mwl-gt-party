{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Main where

------------------------------------------------------------------------------
import           Control.Monad                    (forM)
import qualified Data.Map                         as Map
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           System.FilePath

import           Types

------------------------------------------------------------------------------
main :: IO ()
main = do
  s0@World{..} <- state0
  let inWin = InWindow "Game Theory" (floor width, floor height) (0,0)
      fullScr = FullScreen (floor width, floor height)
  playIO fullScr
    white 30 s0 (return . worldPic) input timestep


------------------------------------------------------------------------------
state0 :: IO World
state0 = do
  let picNames = ["qMark", "coors", "corona", "sam", "leffe", "btnsRequest"]
  pics <- forM picNames $ \n ->
    return . (n,) =<< loadBMP (("pics" </> n) ++ ".bmp")
  return $ World 1280 720 (Map.fromList pics) WaitingBoth


------------------------------------------------------------------------------
-- Input handler
input :: Event -> World -> IO World
input (EventKey (Char k) Down _ _) w@World{..}
  | gameState == WaitingBoth =
      let (p,c) = btnNumberToPlayerChoice k
      in return $ w { gameState = GotOne p c }
  | isGotOne gameState =
        let (GotOne p c) = gameState
            (p',c')      = btnNumberToPlayerChoice k
        in if p' == p 
           then return w -- Ignore repeated press
           else let (cLeft,cRight) = case p of
                      PlayerLeft  -> (c, c')
                      PlayerRight -> (c',c )
                in return $
                   w { gameState = GotBoth cLeft cRight }
  | isConfirmReset gameState = case k of
    'n' -> return $ w {gameState = gameState}
    _   -> return $ w {gameState = WaitingBoth}
  | isGotBoth gameState && k `elem` "1234" = return w
  | isGotBoth gameState = return $ w {gameState = WaitingBoth}
  | gameState == RequestButtonsUp  = return $ w {gameState = WaitingBoth}
input (EventKey (SpecialKey KeySpace) Up _ _ ) w@World{..} =
  return $ w {gameState = WaitingBoth}
input e w = do
  putStrLn $ "Unhandled event " ++ show e
  return w

   
------------------------------------------------------------------------------
worldPic :: World -> Picture
worldPic wrl@World{..}
  | gameState == WaitingBoth =
      case Map.lookup "qMark" resources of
        Nothing -> error "Couldn't load qMark bitmap"
        Just q -> pictures [ picAt wrl PlayerLeft q
                           , picAt wrl PlayerRight q
                           , midLine wrl
                           ]
  | isConfirmReset gameState = text "Really abort trial? y/n"
  | gameState == RequestButtonsUp =
      case Map.lookup "btnsRequest" resources of
        Nothing -> error $ "Couldn't load btnsRequest" ++
                   concat (Map.keys resources)
        Just p  -> p
  | isGotOne gameState =
      case Map.lookup "qMark" resources of
        Nothing -> error "Couldn't load qMark bitmap"
        Just q  -> let (GotOne thePlayer _) = gameState
                       remaining = otherPlayer thePlayer
                   in pictures [ picAt wrl remaining q
                               , midLine wrl
                               ]
  | isGotBoth gameState = let (GotBoth l r) = gameState in
    case map (flip Map.lookup resources) ["coors","corona","sam","leffe"] of
      [Just coors, Just corona, Just sam, Just leffe] -> 
        case (l,r) of
          (Cooperate,Cooperate) -> pictures
            [ picAt wrl PlayerLeft sam
            , picAt wrl PlayerRight sam
            , midLine wrl ]
          (Cheat,Cheat) -> pictures
            [ picAt wrl PlayerLeft corona
            , picAt wrl PlayerRight corona
            , midLine wrl ]
          (Cheat,Cooperate) -> pictures
            [ picAt wrl PlayerLeft leffe
            , picAt wrl PlayerRight coors
            , midLine wrl ]
          (Cooperate,Cheat) -> pictures
            [ picAt wrl PlayerLeft coors
            , picAt wrl PlayerRight leffe
            , midLine wrl ]
      _ -> error "Couldn't load beer pictures."
  | otherwise = error "Impossible case in statePic"


------------------------------------------------------------------------------
picAt :: World -> Player -> Picture -> Picture
picAt World{..} p pic = translate x y pic
  where (x,y) = case p of
          PlayerLeft  -> (-width/4, 0)
          PlayerRight -> ( width/4, 0)


------------------------------------------------------------------------------
midLine :: World -> Picture
midLine World{..} =
  color black $ line [ (0, (-2*height)/5) , (0, (2*height)/5) ]


------------------------------------------------------------------------------
btnNumberToPlayerChoice :: Char -> (Player,Choice)
btnNumberToPlayerChoice '1' = (PlayerLeft,Cooperate)
btnNumberToPlayerChoice '2' = (PlayerLeft,Cheat)
btnNumberToPlayerChoice '3' = (PlayerRight,Cooperate)
btnNumberToPlayerChoice '4' = (PlayerRight,Cheat)
btnNumberToPlayerChoice _   = error "Impossible case"


------------------------------------------------------------------------------
-- Ignore time passing
timestep :: Float -> World -> IO World
timestep _ w = return w


------------------------------------------------------------------------------
isConfirmReset :: GameState -> Bool
isConfirmReset (ConfirmReset _) = True
isConfirmReset _                = False


------------------------------------------------------------------------------
isGotOne :: GameState -> Bool
isGotOne (GotOne _ _) = True
isGotOne _            = False


------------------------------------------------------------------------------
isGotBoth :: GameState -> Bool
isGotBoth (GotBoth _ _ ) = True
isGotBoth _              = False


------------------------------------------------------------------------------
otherPlayer :: Player -> Player
otherPlayer PlayerLeft  = PlayerRight
otherPlayer PlayerRight = PlayerLeft

