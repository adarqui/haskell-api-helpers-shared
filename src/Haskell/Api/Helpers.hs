{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ExplicitForAll    #-}

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



import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.ByteString.Char8      as BSC
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.List                  (dropWhileEnd, intercalate)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import qualified Network.Connection         as Network
import qualified Network.HTTP.Conduit       as Conduit
import           Network.HTTP.Types.Header
import           Network.Wreq
import qualified Network.Wreq.Types         as WreqTypes
import           Prelude                    hiding (log)




type ApiEff = ReaderT ApiOptions IO



data ApiOptions = ApiOptions {
  apiUrl         :: String,
  apiPrefix      :: String,
  apiKey         :: Maybe BSC.ByteString,
  apiKeyHeader   :: Maybe HeaderName,
  apiWreqOptions :: Options,
  apiDebug       :: Bool
} deriving (Show, Generic, Typeable)



data ApiError
  = ServerError Status
  | DecodeError String
  deriving (Show)



class (Show a) => QueryParam a where
  qp :: a -> (String, String)



instance QueryParam (String, String) where
  qp (s,s') = (s, s')



instance QueryParam (Text, Text) where
  qp (t,t') = (T.unpack t, T.unpack t')



route :: String -> [String] -> String
route url paths = intercalate "/" (url : paths)



flattenParams :: QueryParam qp => [qp] -> [String]
flattenParams [] = []
flattenParams params' = map (\par -> let (k,v) = qp par in k ++ "=" ++ v) params'



mkQueryString :: [String] -> String
mkQueryString [] = ""
mkQueryString params' = "?" <> intercalate "&" params'



routeQueryBy :: QueryParam qp => String -> [String] -> [qp] -> String
routeQueryBy url paths params' = route url paths <> mkQueryString (flattenParams params')



runDebug :: ApiEff () -> ApiEff ()
runDebug fn = do
  debug <- asks apiDebug
  if debug
     then do
       fn
       return ()
     else return ()



urlFromReader :: ApiEff String
urlFromReader = do
  ApiOptions{..} <- ask
  let
    apiUrl'    = dropWhileEnd (=='/') apiUrl
    apiPrefix' = dropWhileEnd (=='/') apiPrefix
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



runWithAuthId :: ReaderT ApiOptions m a -> String -> m a
runWithAuthId actions string_id = runWith actions (defaultApiOptions { apiKey = Just $ BSC.pack string_id })



rWA :: ReaderT ApiOptions m a -> String -> m a
rWA = runWithAuthId



paramsToText :: [(String, String)] -> [(Text, Text)]
paramsToText = map (\(a,b) -> (T.pack a, T.pack b))



fixOpts :: [(String, String)] -> ApiEff Options
fixOpts params' = do

  mapi_key <- asks apiKey
  mapi_key_header <- asks apiKeyHeader
  options'  <- asks apiWreqOptions

  let
    opts = case (mapi_key, mapi_key_header) of
      (Just api_key, Just api_key_header) -> options' & header api_key_header .~ [api_key]
      _                                   -> options'

    opts_with_params = Prelude.foldl (\acc (k, v) -> acc & param k .~ [v]) opts $ paramsToText params'

  return $ opts_with_params



handleError ::
  (FromJSON a)
  => Either Status ByteString -> Either ApiError a
handleError (Left status) = Left $ ServerError status
handleError (Right bs)    =
  case eitherDecode bs of
    Left err -> Left $ DecodeError err
    Right a  -> Right a



getAt :: (QueryParam qp)  => [qp] -> [String] -> ApiEff (Either Status ByteString)
getAt params' paths = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (log ("getAt: " <> url'))
  r <- liftIO $ getWith opts url'
  properResponse r



postAt :: (QueryParam qp, ToJSON a) => [qp] -> [String] -> a -> ApiEff (Either Status ByteString)
postAt params' paths body = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (log ("postAt: " <> url'))
  r <- liftIO $ postWith opts url' (toJSON body)
  properResponse r



putAt :: (QueryParam qp, ToJSON a) => [qp] -> [String] -> a -> ApiEff (Either Status ByteString)
putAt params' paths body = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (log ("putAt: " <> url'))
  r <- liftIO $ putWith opts url' (toJSON body)
  properResponse r



deleteAt :: QueryParam qp => [qp] -> [String] -> ApiEff (Either Status ByteString)
deleteAt params' paths = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy url paths params'
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
