{-# LANGUAGE DerivingStrategies #-}

module Main where

import Main.Utf8 qualified as Utf8
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Printf (printf)
import Text.Read (Read (readsPrec))
import Prelude hiding (some)

data Food
  = SalmonAtlantic
  | SausageDuBretonMildItalian -- https://www.dubreton.com/en-ca/products/all-natural/mild-italian-sausages
  | DuBretonBaconBlackForest
  | CostcoKirklandGroundBeef
  | CostcoKirklandScallop
  | CostcoKirklandSockeyeSalmon
  | FontaineLeanGroundVeal
  | Butter
  | Tallow
  | Egg
  | Shrimp
  | PorkBelly
  deriving stock (Show, Eq, Ord)

foodNutrition :: Food -> Nutrition
foodNutrition = \case
  SalmonAtlantic -> read "20p 13f"
  Butter -> read "0.9p 81f 0.1c"
  SausageDuBretonMildItalian -> read "14p 22f 1c"
  DuBretonBaconBlackForest -> read "9p 19f 0c 56g"
  CostcoKirklandGroundBeef -> read "19p 15f"
  CostcoKirklandScallop -> read "21p 0.5f 125g"
  CostcoKirklandSockeyeSalmon -> read "28p 6f 125g"
  FontaineLeanGroundVeal -> read "18p 14f"
  Tallow -> read "0p 100f"
  Egg -> read "13p 10f 0.7c"
  Shrimp -> read "20p 0.3f 0.2c"
  PorkBelly -> read "9p 53f"

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    putStrLn "=> https://calculo.io/keto-calculator"
    putStrLn "   To maintain 156 lbs => 178f & 102p (1.8 ratio)"
    printMealMacros
      "Salmon+Sausage (3 sausages)"
      [ (SalmonAtlantic, 400),
        (SausageDuBretonMildItalian, 300),
        (Butter, 113 * 0.50)
      ]
    printMealMacros
      "Costco Beef+Butter"
      [ (CostcoKirklandGroundBeef, 600),
        (Butter, 113 * 0.50),
        (Tallow, 20)
      ]
    printMealMacros
      "Egg mélange"
      [ (SausageDuBretonMildItalian, 100),
        (Egg, 50 * 6),
        (Shrimp, 130),
        (SalmonAtlantic, 200),
        (Butter, 113 * 0.9)
      ]
    printMealMacros
      "Veal and Pork"
      [ (SausageDuBretonMildItalian, 100),
        (FontaineLeanGroundVeal, 454),
        -- (Tallow, 10),
        (PorkBelly, 100),
        (DuBretonBaconBlackForest, 70)
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
  let rational = fmap (toRational @Double) $ try L.float <|> L.decimal
  protein <- rational <* char 'p'
  void space
  fat <- rational <* char 'f'
  void space
  mcarbs <- optional $ rational <* char 'c'
  let carbs = fromMaybe 0 mcarbs
  let nutrition = Nutrition {protein, fat, carbs}
  void space
  mquantity <- optional $ rational <* char 'g'
  case mquantity of
    Nothing -> pure nutrition
    Just quantity -> pure $ scaleNutritionToGrams' quantity nutrition 100

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

scaleNutritionToGrams' :: Rational -> Nutrition -> Rational -> Nutrition
scaleNutritionToGrams' q (Nutrition p f c) grams =
  Nutrition (p * grams / q) (f * grams / q) (c * grams / q)

nutritionCalories :: Nutrition -> Rational
nutritionCalories (Nutrition p f c) = p * 4 + f * 9 + c * 4

type Meal = [(Food, Rational)]

printMealMacros :: Text -> Meal -> IO ()
printMealMacros title meal = do
  let totalNutrition = sumNutrition meal
  putTextLn $ "## \ESC[1;4m" <> title <> "\ESC[0m"
  let bar n = "\t\ESC[90m" ++ join (replicate (round @_ @Int $ n / 5) "◍") ++ "\ESC[0m"
  forM_ meal $ \(food, quantity) -> do
    putStrLn $ printf "%30s\t=> %ig" (show @Text food) (round @_ @Int $ quantity)
  putStrLn $ "Fat:\t\t" ++ show (round @_ @Int $ fat totalNutrition) ++ bar (fat totalNutrition)
  putStrLn $ "Protein:\t" ++ show (round @_ @Int $ protein totalNutrition) ++ bar (protein totalNutrition)
  putStrLn $ "Carbs:\t\t" ++ show (round @_ @Int $ carbs totalNutrition)
  putStrLn $ "Calories:\t" ++ show (round @_ @Int $ nutritionCalories totalNutrition)
  putStrLn $ "Fat:Protein:\t" ++ printf "\ESC[1m%.2f\ESC[0m" (toDecimal $ fat totalNutrition / protein totalNutrition)
  putStrLn ""

sumNutrition :: Meal -> Nutrition
sumNutrition meal =
  foldl' (\acc (food, quantity) -> acc <> scaleNutritionToGrams food quantity) mempty $
    first foodNutrition <$> meal

read :: (HasCallStack, Read a) => String -> a
read s = fromMaybe (error $ toText $ "Bad input: " ++ s) $ readMaybe s

toDecimal :: Rational -> Double
toDecimal r = fromIntegral (numerator r) / fromIntegral (denominator r)
