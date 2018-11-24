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

data PlayerAction = Ask | Hold deriving (Show, Eq)

data GameState = GameState {
  gPlayers :: Zipper Player,
  gDeck :: [Card],
  gWinstate :: WinState
} deriving (Show)

isGameOver :: GameState -> Bool
isGameOver s = gWinstate s /= None

drawCardForSelected :: Zipper Player -> [Card] -> (Zipper Player, [Card])
drawCardForSelected players deck =
  let selectedPlayer = cursor players
      (card, newDeck) = drawCards 1 deck
      newPlayer = addCards selectedPlayer card
      newPlayers = replace newPlayer players
  in (newPlayers, newDeck)

singlePlayerProcess :: PlayerAction -> GameState -> GameState
singlePlayerProcess Ask s =
  s {
    gPlayers = newPlayers,
    gDeck = newDeck
  }
  where
    (newPlayers, newDeck) = 
      drawCardForSelected (gPlayers s) (gDeck s)
    
singlePlayerProcess Hold s = 
  s { 
    gPlayers = newPlayers,
    gWinstate = newWinState
  }
  where
    newPlayers = right $ gPlayers s
    newWinState = if endp newPlayers
                  then endRoundStuff (toList newPlayers)
                  else None
    
maxOr :: Int -> [Int] -> Int
maxOr a [] = a
maxOr _ aa = maximum aa
    
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
    
addCards :: Player -> [Card] -> Player
addCards p cs = p { pHand = cs ++ pHand p }

loopLogic :: GameState -> IO (Maybe GameState)
loopLogic state = do
    action <- askForAction
    let newState = singlePlayerProcess action state
    if isGameOver newState
    then do
      showState newState
      return Nothing
    else do
      showState newState
      return $ Just newState
        
showState :: GameState -> IO ()
showState s = do
  putStrLn . show . gPlayers $ s
  let selected = safeCursor $ gPlayers s
  maybe (return ()) (\n -> putStrLn $ "Current: " ++ show n) selected
  putStrLn . show . gWinstate $ s  
    
setup :: IO GameState
setup = do
  putStrLn "Playing Single player game"
  putStrLn "Shuffling deck..."
  let names = ["Gyuri", "Petra", "Sapa"]
  deck <- shuffleM gameDeck 
  let (newDeck, players) = foldl myRule (deck, []) names
  let startState =  GameState {
    gPlayers = fromList players,   
    gDeck = newDeck,
    gWinstate = None
  }
  showState startState
  return startState
  where
    gameDeck = concat $ replicate 6 deck 
    myRule (deck, players) name = 
      let (hand, newDeck) = drawCards 2 deck
          newPlayer = Player name hand
      in (newDeck, newPlayer : players)          
        
askForAction :: IO PlayerAction
askForAction = do
  putStrLn "Hold or Ask?"
  putStr "> "
  cmd <- getLine
  case cmd of
    "Ask" -> return Ask
    "Hold" -> return Hold
    _ -> askForAction

main :: IO ()
main = playGame setup loopLogic

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap f a = zip a $ map f a