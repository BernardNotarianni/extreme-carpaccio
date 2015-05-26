{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Text
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.Trans --(liftIO)
import Web.Scotty
import Model

instance FromJSON Order where
  parseJSON (Object v) =
    Order <$>
    (v .: "prices") <*>
    (v .: "quantities") <*>
    (v .: "country") <*>
    (v .: "reduction")

data Feedback = Feedback
    { _feedbackType :: Text
    , _content      :: Text
    } deriving (Show)

instance FromJSON Feedback where
  parseJSON (Object v) =
    Feedback <$>
    (v .: "type") <*>
    (v .: "content")
  parseJSON _ = mzero

instance ToJSON Quantity where
  toJSON (Quantity total) = object [ "total" .= total]

main :: IO ()
main = scotty 8000 $ do

  get "/" $ do
    html "Hello extreme-carpaccio!"

  post "/feedback" $ do
    b <- body
    let f = decode b

    trace (f :: Maybe Feedback)
    html "Thanx for this very usefull feedback"

  post "/order" $ do
    b <- body

    let o = decode b
    trace (o :: Maybe Order)

    let answer = case o of
          Just x -> computeTotal x
          Nothing -> Quantity 0

    trace answer

    raw $ encode $ answer

trace :: (Show a, MonadIO m) => a -> m ()
trace = liftIO . print
