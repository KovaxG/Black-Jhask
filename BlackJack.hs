module BlackJack where

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
          deriving (Show, Enum)

{- Helper functions -}

-- calculates the possible values your hand can have
handValue :: [Card] -> [Score]
handValue = fromValues . mconcat . map value

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

gameDeck :: [Card]
gameDeck = concat $ replicate 6 deck

main :: IO ()
main = undefined
