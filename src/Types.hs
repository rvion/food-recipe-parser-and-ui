{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Aeson.TH
import Data.Text (Text)

data Recipe = Recipe
  { recipeName :: Text
  , ingredients :: [Ingredient]
  , steps :: [Step]
  } deriving Show

type Measure = Text

data Ingredient = Ingredient
  { ingredientName :: Text
  , quantity :: Int
  , measure :: Maybe Measure
  } deriving (Show, Eq)

data Step = Step
  { stepName :: Text
  , order :: Int
  , stepDuration :: Maybe Duration
  } deriving (Eq, Show)

instance Ord Step where
    compare s1 s2 = compare (order s1) (order s2)

data Duration = Duration
  { duration :: Int
  , durationMeasure :: Measure
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Duration)
$(deriveJSON defaultOptions ''Step)
$(deriveJSON defaultOptions ''Ingredient)
$(deriveJSON defaultOptions ''Recipe)
