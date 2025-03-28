{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (makeLenses, (.~))
import Data.Default
import Main.Utf8 qualified as Utf8
import Text.Printf (printf)
import Prelude hiding (some)

data Nutrition = Nutrition
  { _protein :: Rational,
    _fat :: Rational,
    _carbs :: Rational,
    _quantity :: Rational
  }
  deriving stock (Show, Eq, Ord)

makeLenses ''Nutrition

instance Default Nutrition where
  def = Nutrition {_protein = 0, _fat = 0, _carbs = 0, _quantity = 100}

instance Semigroup Nutrition where
  (Nutrition p1 f1 c1 q1) <> (Nutrition p2 f2 c2 q2) =
    Nutrition (p1 + p2) (f1 + f2) (c1 + c2) (q1 + q2)

instance Monoid Nutrition where
  mempty = def & quantity .~ 0

data Food
  = SalmonAtlantic
  | DuBretonSausageFrenchOnion
  | DuBretonBaconBlackForest
  | CostcoKirklandGroundBeef
  | CostcoKirklandScallop
  | CostcoKirklandSockeyeSalmon
  | FontaineLeanGroundVeal
  | Butter
  | Tallow
  | Egg -- A large egg
  | Egg_NutriFreeRunMediumBrown
  | Shrimp
  | PorkBelly
  | PorkBelly_NoRenderedFat
  | Liver
  | QuebonWhippingCream
  | PorkRinds_BakenETS
  | Maxi_GroundBeefMedium_NewZealand
  | StripLoinSteak
  | DarkChocolate_Lindt95
  | AW_Burger_5oz
  deriving stock (Show, Eq, Ord)

{- ORMOLU_DISABLE -}
foodNutrition :: Food -> Nutrition
foodNutrition = \case
  SalmonAtlantic ->
    def & protein .~ 20  & fat .~ 13
  Butter ->
    def & protein .~ 0.9 & fat .~ 81  & carbs .~ 0.1
  DuBretonSausageFrenchOnion ->
    def & protein .~ 14  & fat .~ 22  & carbs .~ 1
  DuBretonBaconBlackForest ->
    def & protein .~ 9   & fat .~ 19                   & quantity .~ 56
  CostcoKirklandGroundBeef ->
    def & protein .~ 19  & fat .~ 15
  CostcoKirklandScallop ->
    def & protein .~ 21  & fat .~ 0.5                  & quantity .~ 125
  CostcoKirklandSockeyeSalmon ->
    def & protein .~ 28  & fat .~ 6                    & quantity .~ 125
  FontaineLeanGroundVeal ->
    def & protein .~ 18  & fat .~ 14
  Tallow ->
    def                  & fat .~ 100
  Egg ->
    def & protein .~ 6  & fat .~ 5  & carbs .~ 0.6     & quantity .~ 50
  Egg_NutriFreeRunMediumBrown ->
    def & protein .~ 11  & fat .~ 9  & carbs .~ 1     & quantity .~ 91
  Shrimp ->
    def & protein .~ 20  & fat .~ 0.3 & carbs .~ 0.2
  PorkBelly ->
    def & protein .~ 9   & fat .~ 53
  PorkBelly_NoRenderedFat ->
    def & protein .~ 9   & fat .~ 31
  Liver ->
    def & protein .~ 26  & fat .~ 4.4 & carbs .~ 3.8
  QuebonWhippingCream ->
    def & protein .~ 0.3 & fat .~ 5   & carbs .~ 1    & quantity .~ 15
  PorkRinds_BakenETS ->
    def & protein .~ 35  & fat .~ 25                  & quantity .~ 70
  Maxi_GroundBeefMedium_NewZealand ->
    def & protein .~ 18  & fat .~ 19                  & quantity .~ 100
  StripLoinSteak ->
    -- https://www.nutritionix.com/food/new-york-strip-steak
    def & protein .~ 22  & fat .~ 19                  & quantity .~ 85
  DarkChocolate_Lindt95 ->
    def & protein .~ 4   & fat .~ 18  & carbs .~ 3    & quantity .~ 32
  AW_Burger_5oz ->
    -- https://www.calorieking.com/us/en/foods/f/calories-in-meal-components-burger-patty/JK2WV-QzRQS8B1_gBkAivg
    def & protein .~ 26.9  & fat .~ 29.3              & quantity .~ 141
{- ORMOLU_ENABLE -}

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    tests
    putStrLn "=> https://calculo.io/keto-calculator"
    putStrLn "   To maintain 153 lbs => 220f & 110p"
    printMealMacros
      "Belly take 2"
      [ (PorkBelly_NoRenderedFat, 300),
        (FontaineLeanGroundVeal, 454),
        (QuebonWhippingCream, 20),
        (Butter, 30.6)
      ]

    printMealMacros
      "Costco Beef+Full Butter"
      [ (CostcoKirklandGroundBeef, 600),
        (Butter, 140),
        (Tallow, 20)
      ]
    printMealMacros
      "Scallop + Sausages"
      [ (DuBretonSausageFrenchOnion, 100 * 4),
        (CostcoKirklandScallop, 160),
        (Egg, 50*4),
        (Butter, 130)
      ]
    printMealMacros
      "Liver + Sausage"
      [ (Liver, 230),
        (DuBretonSausageFrenchOnion, 100 * 4),
        (Butter, 150)
      ]
    printMealMacros
      "Veal and Sausage"
      [ (FontaineLeanGroundVeal, 454),
        (Tallow, 5),
        (DuBretonSausageFrenchOnion, 100 * 2),
        (Butter, 130)
      ]
    printMealMacros
      "Salmom & Sausages"
      [ (SalmonAtlantic, 270),
        (DuBretonSausageFrenchOnion, 400),
        (Butter, 120)
      ]
    printMealMacros
      "Wild Salmom & Sausages"
      [ (CostcoKirklandSockeyeSalmon, 170),
        -- (CostcoKirklandScallop, 50),
        (Egg, 50*6),
        (DuBretonSausageFrenchOnion, 300),
        (Butter, 130)
      ]
    printMealMacros
      "2x egg/sausage & NZ grassfed"
      [ (Maxi_GroundBeefMedium_NewZealand, 450),
        (DuBretonSausageFrenchOnion, 200),
        (Egg_NutriFreeRunMediumBrown, 91*1.5),
        (Tallow, 20),
        (Butter, 90),
        -- 3rd
        (Egg_NutriFreeRunMediumBrown, 91*1.5),
        (Butter, 30)
      ]
    printMealMacros
      "2x: liver/egg & ground beef"
      [ (FontaineLeanGroundVeal, 454),
        (Egg_NutriFreeRunMediumBrown, 91*1.5),
        (Liver, 118),
        (Tallow, 20),
        (Butter, 150)
      ]
    printMealMacros
      "egg meal"
      [
        (Egg_NutriFreeRunMediumBrown, 91*3),
        (Butter, 60),
        (DuBretonSausageFrenchOnion, 200)
      ]

-- ---------------------------------------------
-- Internal
-- ---------------------------------------------

scaleNutritionToGrams :: Nutrition -> Rational -> Nutrition
scaleNutritionToGrams (Nutrition p f c q) grams =
  Nutrition (p * grams / q) (f * grams / q) (c * grams / q) grams

nutritionCalories :: Nutrition -> Rational
nutritionCalories (Nutrition p f c _q) = p * 4 + f * 9 + c * 4

type Meal = [(Food, Rational)]

printMealMacros :: Text -> Meal -> IO ()
printMealMacros title meal = do
  let totalNutrition = sumNutrition meal
  putTextLn $ "## \ESC[1;4m" <> title <> "\ESC[0m" <> " (" <> show (round @_ @Int $ _quantity totalNutrition) <> "g)"
  let bar n = "\t\ESC[90m" ++ join (replicate (round @_ @Int $ n / 5) "◍") ++ "\ESC[0m"
  forM_ meal $ \(food, quantity') -> do
    putStrLn $ printf "%30s\t=> %ig" (show @Text food) (round @_ @Int $ quantity')
  putStrLn $ "Fat:\t\t" ++ show (round @_ @Int $ _fat totalNutrition) ++ bar (_fat totalNutrition)
  putStrLn $ "Protein:\t" ++ show (round @_ @Int $ _protein totalNutrition) ++ bar (_protein totalNutrition)
  putStrLn $ "Carbs:\t\t" ++ show (round @_ @Int $ _carbs totalNutrition)
  putStrLn $ "Calories:\t" ++ show (round @_ @Int $ nutritionCalories totalNutrition)
  putStrLn $ "Fat:Protein:\t" ++ printf "\ESC[1m%.2f\ESC[0m" (toDecimal $ _fat totalNutrition / _protein totalNutrition)
  putStrLn ""

sumNutrition :: Meal -> Nutrition
sumNutrition meal = mconcat nutritions
  where
    nutritions =
      meal <&> \(food, q) ->
        scaleNutritionToGrams (foodNutrition food) q

toDecimal :: Rational -> Double
toDecimal r = fromIntegral (numerator r) / fromIntegral (denominator r)

-- TODO: tidy it up.
tests :: IO ()
tests = do
  let n =
        sumNutrition
          [ (SalmonAtlantic, 400),
            (DuBretonSausageFrenchOnion, 300),
            (Butter, 113 * 0.50)
          ]
      roundTo :: Int -> Double -> Double
      roundTo num x = fromIntegral @Int @Double (round (x * 10 ^ num)) / 10 ^ num
      actual = roundTo 2 $ toDecimal $ _fat n / _protein n
      expected = roundTo 2 1.34
  if actual == expected
    then putStrLn "Test passed"
    else error $ "Test failed: " <> show actual
