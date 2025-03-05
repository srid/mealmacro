{-# LANGUAGE DerivingStrategies #-}

module Main where

import Main.Utf8 qualified as Utf8
import Shower qualified
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Printf (printf)
import Text.Read (Read (readsPrec))
import Prelude hiding (some)

data Food
  = SalmonAtlantic
  | Butter
  | SausageDuBretonMildItalian -- https://www.dubreton.com/en-ca/products/all-natural/mild-italian-sausages
  | CostcoKirklandGroundBeef
  deriving stock (Show, Eq, Ord)

foodNutrition :: Food -> Nutrition
foodNutrition = \case
  SalmonAtlantic -> read "20p 13f"
  Butter -> read "1p 81f"
  SausageDuBretonMildItalian -> read "14p 22f 1c"
  CostcoKirklandGroundBeef -> read "19p 15f"

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    printMealMacros
      [ (SalmonAtlantic, 300),
        (SausageDuBretonMildItalian, 400),
        (Butter, 113 / 2)
      ]
    printMealMacros
      [ (SalmonAtlantic, 350),
        (SausageDuBretonMildItalian, 300),
        (Butter, 113 * 0.6)
      ]
    printMealMacros
      [ (CostcoKirklandGroundBeef, 600),
        (Butter, 113)
      ]

-- ---------------------------------------------
-- Internal
-- ---------------------------------------------

instance Read Nutrition where
  readsPrec _ input = case parse nutritionParser "<>" input of
    Right nutrition -> [(nutrition, "")]
    Left _ -> []

type Parser = Parsec Void String

-- | Parse `12f 13p 1c` into Nutrition {protein=12, fat=13, carbs=1}
-- Use megaparsec to parse the input string into a Nutrition value
parseNutrition :: String -> Maybe Nutrition
parseNutrition input = do
  rightToMaybe $ parse nutritionParser "<>" input

nutritionParser :: Parser Nutrition
nutritionParser = do
  protein <- L.decimal <* char 'p'
  void space
  fat <- L.decimal <* char 'f'
  void space
  mcarbs <- optional $ L.decimal <* char 'c'
  let carbs = fromMaybe 0 mcarbs
  pure $ Nutrition {protein, fat, carbs}

-- Per 100g
data Nutrition = Nutrition
  { protein :: Rational,
    fat :: Rational,
    carbs :: Rational
  }
  deriving stock (Show, Eq, Ord)

baseQuantity :: Rational
baseQuantity = 100

instance Semigroup Nutrition where
  (Nutrition p1 f1 c1) <> (Nutrition p2 f2 c2) = Nutrition (p1 + p2) (f1 + f2) (c1 + c2)

instance Monoid Nutrition where
  mempty = Nutrition 0 0 0

-- FIXME: grams should be part of out data
scaleNutritionToGrams :: Nutrition -> Rational -> Nutrition
scaleNutritionToGrams (Nutrition p f c) grams =
  Nutrition (p * grams / baseQuantity) (f * grams / baseQuantity) (c * grams / baseQuantity)

nutritionCalories :: Nutrition -> Rational
nutritionCalories (Nutrition p f c) = p * 4 + f * 9 + c * 4

type Meal = [(Food, Rational)]

printMealMacros :: Meal -> IO ()
printMealMacros meal = do
  let totalNutrition = sumNutrition meal
  putStrLn $ Shower.shower $ meal <&> second (round @_ @Int)
  putStrLn "Total Nutrition:"
  putStrLn $ "Protein: " ++ show (round @_ @Int $ protein totalNutrition)
  putStrLn $ "Fat: " ++ show (round @_ @Int $ fat totalNutrition)
  putStrLn $ "Carbs: " ++ show (toDecimal $ carbs totalNutrition)
  putStrLn $ "Calories: " ++ show (round @_ @Int $ nutritionCalories totalNutrition)
  putStrLn $ "Fat:Protein ratio: " ++ printf "%.2f" (toDecimal $ fat totalNutrition / protein totalNutrition)
  putStrLn "----"

sumNutrition :: Meal -> Nutrition
sumNutrition meal =
  foldl' (\acc (food, quantity) -> acc <> scaleNutritionToGrams food quantity) mempty $
    first foodNutrition <$> meal

read :: (HasCallStack, Read a) => String -> a
read = fromMaybe (error "Bad input") . readMaybe

toDecimal :: Rational -> Double
toDecimal r = fromIntegral (numerator r) / fromIntegral (denominator r)
