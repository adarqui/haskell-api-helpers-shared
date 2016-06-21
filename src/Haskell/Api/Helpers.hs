{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Haskell.Api.Helpers (
  ApiOptions (..),
  ApiError (..),
  ApiEff,
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



import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import           Data.Aeson                 (FromJSON, ToJSON, eitherDecode,
                                             toJSON)
import qualified Data.ByteString.Char8      as BSC (ByteString)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Monoid                ((<>))
import           Data.String.Conversions    (ConvertibleStrings, convertString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (dropWhileEnd, intercalate)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import qualified Network.Connection         as Network (TLSSettings (..))
import qualified Network.HTTP.Conduit       as Conduit (ManagerSettings,
                                                        mkManagerSettings)
import           Network.HTTP.Types.Header  (HeaderName)
import           Network.Wreq               (Options, Response, Status,
                                             defaults, deleteWith, getWith,
                                             header, param, postWith, putWith,
                                             responseBody, responseStatus,
                                             statusCode)
import qualified Network.Wreq.Types         as WreqTypes (Options (..), manager)
import           Prelude                    hiding (log)




type ApiEff = ReaderT ApiOptions IO



data ApiOptions = ApiOptions {
  apiUrl         :: Text,
  apiPrefix      :: Text,
  apiKey         :: Maybe BSC.ByteString,
  apiKeyHeader   :: Maybe HeaderName,
  apiWreqOptions :: Options,
  apiDebug       :: Bool
} deriving (Show, Generic, Typeable)



data ApiError
  = ServerError Status
  | DecodeError Text
  deriving (Show)



class QueryParam a where
  qp :: a -> (Text, Text)



instance QueryParam (Text, Text) where
  qp (t,t') = (t, t')



cs :: Data.String.Conversions.ConvertibleStrings a b => a -> b
cs = convertString



route :: Text -> [Text] -> Text
route url paths = T.intercalate "/" (url : paths)



flattenParams :: QueryParam qp => [qp] -> [Text]
flattenParams [] = []
flattenParams params' = map (\par -> let (k,v) = qp par in k <> "=" <> v) params'



mkQueryString :: [Text] -> Text
mkQueryString [] = ""
mkQueryString params' = "?" <> T.intercalate "&" params'



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
       return ()
     else return ()



urlFromReader :: ApiEff Text
urlFromReader = do
  ApiOptions{..} <- ask
  let
    apiUrl'    = T.dropWhileEnd (=='/') apiUrl
    apiPrefix' = T.dropWhileEnd (=='/') apiPrefix
  return $ apiUrl' <> "/" <> apiPrefix'



settings :: Conduit.ManagerSettings
settings = Conduit.mkManagerSettings (Network.TLSSettingsSimple True False False) Nothing



defaultWreqOptions :: WreqTypes.Options
defaultWreqOptions = defaults {
  WreqTypes.manager = Left settings -- Left tlsManagerSettings
}



defaultApiOptions :: ApiOptions
defaultApiOptions = ApiOptions {
  apiUrl = "https://github.com",
  apiPrefix = "api",
  apiKey = Nothing,
  apiKeyHeader = Nothing,
  apiWreqOptions = defaultWreqOptions,
  apiDebug = True
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



-- paramsToText :: [(Text, Text)] -> [(Text, Text)]
-- paramsToText = map (\(a,b) -> (cs a, cs b))



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

  return $ opts_with_params



handleError ::
  (FromJSON a)
  => Either Status ByteString -> Either ApiError a
handleError (Left status) = Left $ ServerError status
handleError (Right bs)    =
  case eitherDecode bs of
    Left err -> Left $ DecodeError $ cs err
    Right a  -> Right a



getAt :: (QueryParam qp)  => [qp] -> [Text] -> ApiEff (Either Status ByteString)
getAt params' paths = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy' url paths params'
  runDebug (log ("getAt: " <> url'))
  r <- liftIO $ getWith opts url'
  properResponse r



postAt :: (QueryParam qp, ToJSON a) => [qp] -> [Text] -> a -> ApiEff (Either Status ByteString)
postAt params' paths body = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy' url paths params'
  runDebug (log ("postAt: " <> url'))
  r <- liftIO $ postWith opts url' (toJSON body)
  properResponse r



putAt :: (QueryParam qp, ToJSON a) => [qp] -> [Text] -> a -> ApiEff (Either Status ByteString)
putAt params' paths body = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy' url paths params'
  runDebug (log ("putAt: " <> url'))
  r <- liftIO $ putWith opts url' (toJSON body)
  properResponse r



deleteAt :: QueryParam qp => [qp] -> [Text] -> ApiEff (Either Status ByteString)
deleteAt params' paths = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy' url paths params'
  runDebug (log ("deleteAt: " <> url'))
  r <- liftIO $ deleteWith opts url'
  properResponse r



properResponse :: Monad m => Response body -> m (Either Status body)
properResponse r = do
  case (r ^. responseStatus ^. statusCode) of
    200 -> return $ Right (r ^. responseBody)
    _   -> return $ Left (r ^. responseStatus)



log :: MonadIO m => String -> m ()
log s = liftIO $ putStrLn s
