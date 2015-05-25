module Model
where

data Order = Order
    { prices :: [Double]
    , quantities :: [Integer]
    , country :: String
    , reduction :: String
    } deriving (Show)

data Quantity = Quantity
    { _total :: Double } deriving (Show,Eq)

computeTotal :: Order -> Quantity
computeTotal order =
  Quantity total
  where
    basket = zip (prices order) (quantities order)
    t = vat (country order) / 100.0
    beforeReduc = sum ([p * fromInteger(q) | (p,q) <- basket]) * (1+t)
    total = round100 $ beforeReduc `withReduction` reduction order


withReduction :: Double -> String -> Double
withReduction amount "HALF PRICE" = amount / 2.0
withReduction amount _ = amount * (1 - (standardReduction amount)/100.0)

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

vat :: String -> Double
vat "DE" = 20
vat "UK" = 21
vat "FR" = 20
vat "IT" = 25
vat "ES" = 19
vat "PL" = 21
vat "RO" = 20
vat "NL" = 20
vat "BE" = 24
vat "EL" = 20
vat "CZ" = 19
vat "PT" = 23
vat "HU" = 27
vat "SE" = 23
vat "AT" = 22
vat "BG" = 21
vat "DK" = 21
vat "FI" = 17
vat "SK" = 18
vat "IE" = 21
vat "HR" = 23
vat "LT" = 23
vat "SI" = 24
vat "LV" = 20
vat "EE" = 22
vat "CY" = 21
vat "LU" = 25
vat "MT" = 20
vat _    = 0.0
