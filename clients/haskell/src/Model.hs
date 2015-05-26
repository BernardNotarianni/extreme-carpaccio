module Model
where

data Order = Order
    { prices :: [Double]
    , quantities :: [Integer]
    , country :: String
    , reductionCode :: String
    } deriving (Show)

data Quantity = Quantity
    { _total :: Double } deriving (Show,Eq)

computeTotal :: Order -> Quantity
computeTotal order =
  Quantity totalWithReduction
  where
    basket =  prices order `zip` quantities order
    totalWithoutVat = sum ([p * fromInteger(q) | (p,q) <- basket])
    vat = totalWithoutVat * vatRate (country order) / 100.0
    totalWithVat = totalWithoutVat + vat
    totalWithReduction = round100 $ totalWithVat `applyReduction` (reductionCode order)



applyReduction :: Double -> String -> Double
applyReduction amount "HALF PRICE" = amount / 2.0
applyReduction amount _ = amount * (1 - (standardReduction amount)/100.0)

standardReduction :: Double -> Double
standardReduction total
  | total >= 50000 = 15
  | total >= 10000 = 10
  | total >= 7000 = 7
  | total >= 5000 = 5
  | total >= 1000 = 3
  | otherwise = 0

round100 :: Double -> Double
round100 x = (fromInteger $ round $ x * 100) / (100.0)

vatRate :: String -> Double
vatRate "DE" = 20
vatRate "UK" = 21
vatRate "FR" = 20
vatRate "IT" = 25
vatRate "ES" = 19
vatRate "PL" = 21
vatRate "RO" = 20
vatRate "NL" = 20
vatRate "BE" = 24
vatRate "EL" = 20
vatRate "CZ" = 19
vatRate "PT" = 23
vatRate "HU" = 27
vatRate "SE" = 23
vatRate "AT" = 22
vatRate "BG" = 21
vatRate "DK" = 21
vatRate "FI" = 17
vatRate "SK" = 18
vatRate "IE" = 21
vatRate "HR" = 23
vatRate "LT" = 23
vatRate "SI" = 24
vatRate "LV" = 20
vatRate "EE" = 22
vatRate "CY" = 21
vatRate "LU" = 25
vatRate "MT" = 20
vatRate _    = 0.0
