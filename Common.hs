module Common where

{- Type definitions -}

type Score = Int

data Value = Only Score | Both Value Value deriving (Show)

data Card = Ace
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          deriving (Show, Enum, Eq)

{- Helper functions -}

-- calculates the possible values your hand can have
handValue :: [Card] -> [Score]
handValue = filter (<=21) . fromValues . mconcat . map value

instance Semigroup Value where
  (<>) = (|+|)

instance Monoid Value where
  mempty = Only 0 -- kind of true

(|+|) :: Value -> Value -> Value
Only a |+| Only b = Only $ a + b
a@(Only _) |+| Both b c = Both (a |+| b) (a |+| c)
Both a b |+| c@(Only _) = Both (a |+| c) (b |+| c)
Both a b |+| Both c d = Both (Both (a |+| c) (a |+| d)) (Both (b |+| c) (b |+| d))

fromValues :: Value -> [Score]
fromValues (Only a) = [a]
fromValues (Both a b) = fromValues a ++ fromValues b

value :: Card -> Value
value card = case card of
  Ace   -> Both (Only 1) (Only 11)
  Two   -> Only 2
  Three -> Only 3
  Four  -> Only 4
  Five  -> Only 5
  Six   -> Only 6
  Seven -> Only 7
  Eight -> Only 8
  Nine  -> Only 9
  _     -> Only 10

{- Game stuff -}

deck :: [Card]
deck = concat $ replicate 4 [Ace .. King]

drawCards :: Int -> [Card] -> ([Card], [Card])
drawCards nr deck = (take nr deck, drop nr deck)

playGame :: IO state -> (state -> IO (Maybe state)) -> IO ()
playGame setup loopLogic = do
  initialState <- setup
  loop initialState
  where 
    loop state = do
      newState <- loopLogic state
      maybe (putStrLn "End of Game") loop newState