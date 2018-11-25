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
  putStrLn "\nShuffling deck..."
  deck <- shuffleM gameDeck
  let (newDeck, players) = foldl deal (deck, []) names
  let startState =  GameState {
    gPlayers = fromList players,
    gDeck = newDeck,
    gWinstate = None
  }
  showStates players
  showState startState []
  return startState
  where
    gameDeck = concat $ replicate 6 deck

    deal (deck, players) name =
      let (hand, newDeck) = drawCards 2 deck
          newPlayer = Player name hand
      in (newDeck, players ++ [newPlayer])

    showStates players = mapM_ printPlayer players
      where
        printPlayer player =
          let playerHandValue = show . handValue . pHand $ player
          in putStrLn $ show player ++ " - " ++ playerHandValue


getPlayerNames :: IO [String]
getPlayerNames = do
  putStrLn "How many players?"
  nrMaybe <- safeRead <$> getLine
  maybe getPlayerNames askForNames nrMaybe
  where
    askForNames nr = do
      putStrLn "Please add a name for each player"
      let indexes = [1 .. nr]
      let readLines = replicate nr getLine
      sequence $ zipWith
        (\i r -> putStr (show i ++ ": ") >> r)
        indexes
        readLines


showState :: GameState -> [GameOutput] -> IO ()
showState state outputs = do
  newLine
  mapM_ (putStrLn . (++) "-> " . show) outputs
  newLine
  maybe noOp showSelected . safeCursor . gPlayers $ state
  where
    newLine = putStrLn ""
    noOp = return ()
    showSelected player =
      let playerHandValue = show . handValue . pHand $ player
      in putStrLn $ "Current: " ++ show player ++ " - " ++ playerHandValue


loopLogic :: GameState -> IO (Maybe GameState)
loopLogic state = do
    action <- askForAction (pName . cursor . gPlayers $ state)
    let (newState, outputs) = singlePlayerProcess action state
    if isGameOver newState
    then do
      showState newState outputs
      return Nothing
    else do
      showState newState outputs
      return $ Just newState
    where
      isGameOver = (/=) None . gWinstate


askForAction :: String -> IO PlayerAction
askForAction name = do
  putStrLn "Hold or Ask?"
  putStr $ name ++ " > "
  cmdMaybe <- safeRead <$> getLine
  maybe (askForAction name) return cmdMaybe


singlePlayerProcess :: PlayerAction -> GameState -> (GameState, [GameOutput])
singlePlayerProcess Ask state =
  let (newPlayers, newDeck, outputs1) =
        drawCardForSelected (gPlayers state) (gDeck state)
      (newPlayers2, newWinState, outputs2) = endTurnIfBust newPlayers
      newState = state {
        gPlayers = newPlayers2,
        gDeck = newDeck,
        gWinstate = newWinState
      }
  in (newState, outputs1 ++ outputs2)
singlePlayerProcess Hold state =
  let (newPlayers, newWinState, outputs) = focusOnNextPlayer (gPlayers state)
      newState =  state {
        gPlayers = newPlayers,
        gWinstate = newWinState
      }
  in (newState, outputs)


drawCardForSelected :: Zipper Player
                    -> [Card]
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
     then let (newPlayers, newWinState, outputs) = focusOnNextPlayer players
          in (newPlayers, newWinState, [Bust] ++ outputs)
     else (players, None, [])
  where
    isBusted = null . handValue . pHand


focusOnNextPlayer :: Zipper Player -> (Zipper Player, WinState, [GameOutput])
focusOnNextPlayer players = (newPlayers, newWinState, outputs)
  where
    newPlayers = right players
    (newWinState, outputs) = if endp newPlayers
                             then let ws = endRoundStuff (toList newPlayers)
                                  in (ws, [EndGame ws])
                             else (None, [])


endRoundStuff :: [Player] -> WinState
endRoundStuff = foldl updateWinState None
  where
    bestValue = maxOr 0 . handValue . pHand

    updateWinState None p = PlayerWin p
    updateWinState (PlayerWin p2) p1 =
      case compare (bestValue p1) (bestValue p2) of
        EQ -> Draw [p1, p2]
        LT -> PlayerWin p2
        GT -> PlayerWin p1
    updateWinState (Draw ps) p =
      case compare (bestValue . head $ ps) (bestValue p) of
        EQ -> Draw $ p : ps
        LT -> PlayerWin p
        GT -> Draw ps
