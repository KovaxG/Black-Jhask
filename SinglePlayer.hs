module SinglePlayer where

import Common
import Data.List.Zipper 
import Debug.Trace
import System.Random.Shuffle

data Player = Player {
  pName :: String,
  pHand :: [Card]
} deriving (Show, Eq)

data WinState = None | PlayerWin Player | Draw [Player] deriving (Show, Eq)

data PlayerAction = Ask | Hold deriving (Show, Read, Eq)

data GameOutput = NewCard Card | Bust | EndGame WinState deriving (Show)

data GameState = GameState {
  gPlayers :: Zipper Player,
  gDeck :: [Card],
  gWinstate :: WinState
} deriving (Show)

main :: IO ()
main = playGame setup loopLogic

setup :: IO GameState
setup = do
  putStrLn "Playing Single player game"
  names <- getPlayerNames
  putStrLn "Shuffling deck..."
  deck <- shuffleM gameDeck 
  let (newDeck, players) = foldl deal (deck, []) names
  let startState =  GameState {
    gPlayers = fromList players,   
    gDeck = newDeck,
    gWinstate = None
  }
  showState startState []
  return startState
  where
    gameDeck = concat $ replicate 6 deck 
    deal (deck, players) name = 
      let (hand, newDeck) = drawCards 2 deck
          newPlayer = Player name hand
      in (newDeck, newPlayer : players)

getPlayerNames :: IO [String]
getPlayerNames = do
  putStrLn "How many players?"
  nrMaybe <- safeRead <$> getLine
  maybe getPlayerNames askForNames nrMaybe
  where
    askForNames nr = do
      putStrLn "Please add a name for each player"
      sequence $ replicate nr getLine

showState :: GameState -> [GameOutput] -> IO ()
showState state output = do
  let players = gPlayers state
  putStrLn ""
  mapM_ (putStrLn . (++) "-> " . show) output
  putStrLn ""
  showStates (toList players)
  putStrLn ""
  maybe noOp showSelected . safeCursor $ players
  where
    noOp = return ()
    showSelected player = 
      let playerHandValue = show . handValue . pHand $ player
      in putStrLn $ "Current: " ++ show player ++ " - " ++ playerHandValue
    showStates players = 
      mapM_ forEach players
      where
        forEach player =
          let playerHandValue = show . handValue . pHand $ player
          in putStrLn $ show player ++ " - " ++ playerHandValue
      
loopLogic :: GameState -> IO (Maybe GameState)
loopLogic state = do
    action <- askForAction
    let (newState, output) = singlePlayerProcess action state
    if isGameOver newState
    then do
      showState newState output
      return Nothing 
    else do
      showState newState output
      return $ Just newState
    where
      isGameOver = (/=) None . gWinstate  
     
askForAction :: IO PlayerAction
askForAction = do
  putStr "Hold or Ask?\n> "
  cmdMaybe <- safeRead <$> getLine
  maybe askForAction return cmdMaybe
  
singlePlayerProcess :: PlayerAction -> GameState -> (GameState, [GameOutput])
singlePlayerProcess Ask state =
  let (newPlayers, newDeck, out1) = 
        drawCardForSelected (gPlayers state) (gDeck state) 
      (newPlayers2, newWinState, out2) = endTurnIfBust newPlayers
      newState = state {
        gPlayers = newPlayers2,
        gDeck = newDeck,
        gWinstate = newWinState
      }  
  in (newState, out1 ++ out2)  
singlePlayerProcess Hold state =
  let (newPlayers, newWinState, out) = focusOnNextPlayer (gPlayers state)
      newState =  state { 
        gPlayers = newPlayers,
        gWinstate = newWinState
      }
  in (newState, out)

drawCardForSelected :: Zipper Player -> [Card] 
                    -> (Zipper Player, [Card], [GameOutput])
drawCardForSelected players deck =
  let selectedPlayer = cursor players
      (card, newDeck) = drawCards 1 deck
      newPlayer = addCards selectedPlayer card
      newPlayers = replace newPlayer players
  in (newPlayers, newDeck, [NewCard $ head card])
  where
    addCards :: Player -> [Card] -> Player
    addCards p cs = p { pHand = cs ++ pHand p }

endTurnIfBust :: Zipper Player -> (Zipper Player, WinState, [GameOutput])
endTurnIfBust players = 
  let selected = cursor players
  in if isBusted selected
     then let (newPlayers, newWinState, out) = focusOnNextPlayer players
          in (newPlayers, newWinState, [Bust] ++ out)
     else (players, None, [])
  where
    isBusted p = null . handValue . pHand $ p
    
focusOnNextPlayer :: Zipper Player -> (Zipper Player, WinState, [GameOutput])
focusOnNextPlayer players = (newPlayers, newWinState, out)
  where 
    newPlayers = right players
    (newWinState, out) = if endp newPlayers
                         then let ws = endRoundStuff (toList newPlayers)
                              in (ws, [EndGame ws])
                         else (None, [])
    
endRoundStuff :: [Player] -> WinState
endRoundStuff = foldl rule None
  where
    rule None p = PlayerWin p
    rule (PlayerWin p2) p1 = 
      let p1Value = maxOr 0 . handValue $ pHand p1
          p2Value = maxOr 0 . handValue $ pHand p2
      in case (compare p1Value p2Value) of
        EQ -> Draw [p1, p2]
        LT -> PlayerWin p2
        GT -> PlayerWin p1
    rule (Draw ps) p = 
      let pDValue = maxOr 0 . handValue . pHand . head $ ps
          pValue = maxOr 0 . handValue . pHand $ p
      in case (compare pDValue pValue) of
        EQ -> Draw $ p : ps
        LT -> PlayerWin p
        GT -> Draw ps
