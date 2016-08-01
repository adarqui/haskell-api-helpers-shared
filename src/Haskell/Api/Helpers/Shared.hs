{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Haskell.Api.Helpers.Shared (
  ApiOptions (..),
  ApiError (..),
  ApiEff,
  RawApiResult,
  QueryParam (..),
  defaultApiOptions,
  route,
  flattenParams,
  mkQueryString,
  routeQueryBy,
  routeQueryBy',
  runDebug,
  urlFromReader,
  runDefault,
  rD,
  runWith,
  rW,
  runWithAuthId,
  rWA,
  apiLog
) where



import           Control.DeepSeq            (NFData)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import qualified Data.ByteString.Char8      as BSC (ByteString)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Default               (Default, def)
import           Data.Monoid                ((<>))
import           Data.String.Conversions    (ConvertibleStrings, cs)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text (dropWhileEnd, intercalate)
import qualified Data.Text.IO               as TextIO
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Network.HTTP.Types         (Status)
import           Network.HTTP.Types.Header  (HeaderName)



type ApiEff options       = ReaderT (ApiOptions options) IO



-- | Raw API Result, which can include an Error + Message, or the Response Body
-- (Status, ByteString) is redundant because Status contains a statusMessage (ByteString).
-- However, we are potentially pulling the response body from the content of the header: X-jSON-ERROR.
-- This is because we don't have access to the body of the message in an exception.
--
type RawApiResult = Either (Status, ByteString) ByteString



data ApiOptions options = ApiOptions {
  apiUrl       :: Text,
  apiPrefix    :: Text,
  apiKey       :: Maybe BSC.ByteString,
  apiKeyHeader :: Maybe HeaderName,
  apiOptions   :: options,
  apiDebug     :: Bool
} deriving (Show, Generic, Typeable, NFData)

instance Default options => Default (ApiOptions options) where
  def = defaultApiOptions



data ApiError b
  = ServerError Status b
  | DecodeError Text
  deriving (Show)



class QueryParam a where
  qp :: a -> (Text, Text)



instance QueryParam (Text, Text) where
  qp (t,t') = (t, t')



route :: Text -> [Text] -> Text
route url paths = Text.intercalate "/" (url : paths)



flattenParams :: QueryParam qp => [qp] -> [Text]
flattenParams [] = []
flattenParams params' = map (\par -> let (k,v) = qp par in k <> "=" <> v) params'



mkQueryString :: [Text] -> Text
mkQueryString [] = ""
mkQueryString params' = "?" <> Text.intercalate "&" params'



routeQueryBy :: QueryParam qp => Text -> [Text] -> [qp] -> Text
routeQueryBy url paths params' = route url paths <> mkQueryString (flattenParams params')



routeQueryBy' :: QueryParam qp => Text -> [Text] -> [qp] -> String
routeQueryBy' url paths params' = cs $ routeQueryBy url paths params'



runDebug :: ApiEff options () -> ApiEff options ()
runDebug fn = do
  debug <- asks apiDebug
  if debug
     then do
       fn
       pure ()
     else pure ()



urlFromReader :: ApiEff options Text
urlFromReader = do
  ApiOptions{..} <- ask
  let
    apiUrl'    = Text.dropWhileEnd (=='/') apiUrl
    apiPrefix' = Text.dropWhileEnd (=='/') apiPrefix
  pure $ apiUrl' <> "/" <> apiPrefix'



defaultApiOptions :: Default options => ApiOptions options
defaultApiOptions = ApiOptions {
  apiUrl         = "/",
  apiPrefix      = "api",
  apiKey         = Nothing,
  apiKeyHeader   = Nothing,
  apiOptions     = def,
  apiDebug       = True
}



runDefault :: Default options => ReaderT (ApiOptions options) m a -> m a
runDefault actions = runReaderT actions def



rD :: Default options => ReaderT (ApiOptions options) m a -> m a
rD = runDefault



runWith :: Default options => ReaderT (ApiOptions options) m a -> ApiOptions options -> m a
runWith actions state = runReaderT actions state



rW :: Default options => ReaderT (ApiOptions options) m a -> ApiOptions options -> m a
rW = runWith



runWithAuthId :: Default options => ReaderT (ApiOptions options) m a -> Text -> m a
runWithAuthId actions string_id = runWith actions (defaultApiOptions { apiKey = Just $ cs string_id })



rWA :: Default options => ReaderT (ApiOptions options) m a -> Text -> m a
rWA = runWithAuthId



apiLog :: (MonadIO m, ConvertibleStrings s Text) => s -> m ()
apiLog s = liftIO $ TextIO.putStrLn $ cs s
