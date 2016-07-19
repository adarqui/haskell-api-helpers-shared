{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Haskell.Api.Helpers (
  ApiOptions (..),
  ApiError (..),
  ApiEff,
  RawApiResult,
  QueryParam (..),
  defaultApiOptions,
  settings,
  defaultWreqOptions,
  route,
  flattenParams,
  mkQueryString,
  routeQueryBy,
  routeQueryBy',
  runDebug,
  urlFromReader,
  handleError,
  getAt,
  postAt,
  putAt,
  deleteAt,
  runDefault,
  rD,
  runWith,
  rW,
  runWithAuthId,
  rWA
) where



import           Control.Exception          (catch)
import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import           Data.Aeson                 (FromJSON, ToJSON, eitherDecode,
                                             toJSON)
import qualified Data.ByteString.Char8      as BSC (ByteString)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Default
import           Data.List                  (find)
import           Data.Monoid                ((<>))
import           Data.String.Conversions    (cs)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text (dropWhileEnd, intercalate)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import qualified Network.Connection         as Network (TLSSettings (..))
import           Network.HTTP.Client        (HttpException (..))
import qualified Network.HTTP.Conduit       as Conduit (ManagerSettings,
                                                        mkManagerSettings)
import           Network.HTTP.Types.Header  (HeaderName)
import           Network.HTTP.Types.Status  (status500, statusMessage)
import           Network.Wreq               (Options, Response, Status,
                                             defaults, deleteWith, getWith,
                                             header, param, postWith, putWith,
                                             responseBody, responseStatus,
                                             statusCode)
import qualified Network.Wreq.Types         as WreqTypes (Options (..), manager)
import           Prelude                    hiding (log)




type ApiEff       = ReaderT ApiOptions IO


-- | Raw API Result, which can include an Error + Message, or the Response Body
-- (Status, ByteString) is redundant because Status contains a statusMessage (ByteString).
-- However, we are potentially pulling the response body from the content of the header: X-jSON-ERROR.
-- This is because we don't have access to the body of the message in an exception.
--
type RawApiResult = Either (Status, ByteString) ByteString



data ApiOptions = ApiOptions {
  apiUrl         :: Text,
  apiPrefix      :: Text,
  apiKey         :: Maybe BSC.ByteString,
  apiKeyHeader   :: Maybe HeaderName,
  apiWreqOptions :: Options,
  apiDebug       :: Bool
} deriving (Show, Generic, Typeable)



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



runDebug :: ApiEff () -> ApiEff ()
runDebug fn = do
  debug <- asks apiDebug
  if debug
     then do
       fn
       pure ()
     else pure ()



urlFromReader :: ApiEff Text
urlFromReader = do
  ApiOptions{..} <- ask
  let
    apiUrl'    = Text.dropWhileEnd (=='/') apiUrl
    apiPrefix' = Text.dropWhileEnd (=='/') apiPrefix
  pure $ apiUrl' <> "/" <> apiPrefix'



settings :: Conduit.ManagerSettings
settings = Conduit.mkManagerSettings (Network.TLSSettingsSimple True False False) Nothing



defaultWreqOptions :: WreqTypes.Options
defaultWreqOptions = defaults {
  WreqTypes.manager = Left settings -- Left tlsManagerSettings
}



defaultApiOptions :: ApiOptions
defaultApiOptions = ApiOptions {
  apiUrl         = "https://github.com",
  apiPrefix      = "api",
  apiKey         = Nothing,
  apiKeyHeader   = Nothing,
  apiWreqOptions = defaultWreqOptions,
  apiDebug       = True
}



runDefault :: ReaderT ApiOptions m a -> m a
runDefault actions = runReaderT actions defaultApiOptions



rD :: ReaderT ApiOptions m a -> m a
rD = runDefault



runWith :: ReaderT ApiOptions m a -> ApiOptions -> m a
runWith actions state = runReaderT actions state



rW :: ReaderT ApiOptions m a -> ApiOptions -> m a
rW = runWith



runWithAuthId :: ReaderT ApiOptions m a -> Text -> m a
runWithAuthId actions string_id = runWith actions (defaultApiOptions { apiKey = Just $ cs string_id })



rWA :: ReaderT ApiOptions m a -> Text -> m a
rWA = runWithAuthId



fixOpts :: [(Text, Text)] -> ApiEff Options
fixOpts params' = do

  mapi_key <- asks apiKey
  mapi_key_header <- asks apiKeyHeader
  options'  <- asks apiWreqOptions

  let
    opts = case (mapi_key, mapi_key_header) of
      (Just api_key, Just api_key_header) -> options' & header api_key_header .~ [api_key]
      _                                   -> options'

    opts_with_params = Prelude.foldl (\acc (k, v) -> acc & param k .~ [v]) opts params'

  pure $ opts_with_params



_eitherDecode :: (FromJSON a, Default a) => ByteString -> a
_eitherDecode bs =
  case eitherDecode bs of
    Left _  -> def
    Right v -> v



handleError :: (FromJSON a, FromJSON b, Default b) => RawApiResult -> Either (ApiError b) a
handleError (Left (status, body)) = Left $ ServerError status (_eitherDecode body)
handleError (Right bs)    =
  case eitherDecode bs of
    Left err -> Left $ DecodeError $ cs err
    Right a  -> Right a



internalAction
  :: forall (m :: * -> *).
     (MonadIO m)
  => IO (Response ByteString)
  -> m RawApiResult
internalAction act = liftIO ((act >>= properResponse) `catch` handler)
  where
  handler (StatusCodeException s headers _) = do
     -- This basically makes this library specific to my ln-* project.
     -- It looks for the X-jSON-ERROR header, and if it is set, returns
     -- that message. This message may then be a JSON string, which can give us
     -- more detailed error information.
     --
     case find ((==) "X-jSON-ERROR" . fst) headers of
       Nothing        -> pure $ Left (s, cs $ statusMessage s)
       Just (_, body) -> pure $ Left (s, cs body)
  handler _                                 = pure $ Left (status500, "wreq")



properResponse :: Monad m => Response ByteString -> m RawApiResult
properResponse r = do
  case (r ^. responseStatus ^. statusCode) of
    200 -> pure $ Right (r ^. responseBody)
    _   -> pure $ Left ((r ^. responseStatus), (r ^. responseBody))



getAt :: (QueryParam qp)  => [qp] -> [Text] -> ApiEff RawApiResult
getAt params' paths = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy' url paths params'
  runDebug (log ("getAt: " <> url'))
  internalAction $ getWith opts url'



postAt :: (QueryParam qp, ToJSON a) => [qp] -> [Text] -> a -> ApiEff RawApiResult
postAt params' paths body = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy' url paths params'
  runDebug (log ("postAt: " <> url'))
  internalAction $ postWith opts url' (toJSON body)



putAt :: (QueryParam qp, ToJSON a) => [qp] -> [Text] -> a -> ApiEff RawApiResult
putAt params' paths body = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy' url paths params'
  runDebug (log ("putAt: " <> url'))
  internalAction $ putWith opts url' (toJSON body)



deleteAt :: QueryParam qp => [qp] -> [Text] -> ApiEff RawApiResult
deleteAt params' paths = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy' url paths params'
  runDebug (log ("deleteAt: " <> url'))
  internalAction $ deleteWith opts url'



log :: MonadIO m => String -> m ()
log s = liftIO $ putStrLn s
