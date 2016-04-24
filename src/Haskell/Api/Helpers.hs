{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskell.Api.Helpers (
  ApiOptions (..),
  ApiEff,
  By (..),
  defaultApiOptions,
  settings,
  defaultWreqOptions,
  route,
  flattenParams,
  mkQueryString,
  routeQueryBy,
  runDebug,
  urlFromReader,
  getAt,
  postAt,
  putAt,
  deleteAt,
  runDefault,
  runWith,
  runWithAuthId
) where



import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
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



class (Show a) => By a where
  by :: a -> String



instance By (String, String) where
  by (s,s') = s <> "=" <> s'



instance By (Text, Text) where
  by (t,t') = T.unpack $ t <> ("=" :: Text) <> t'



route :: String -> [String] -> String
route url paths = intercalate "/" (url : paths)



flattenParams :: [(String, String)] -> [String]
flattenParams [] = []
flattenParams params' = map (\(k, v) -> k <> "=" <> v) params'



mkQueryString :: [String] -> String
mkQueryString [] = ""
mkQueryString params' = "?" <> intercalate "&" params'



routeQueryBy :: By by => String -> [String] -> [(String,String)] -> [by] -> String
routeQueryBy url paths params' by' = route url paths <> mkQueryString (by'' <> flattenParams params')
  where
  by'' = map show by'



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



runWith :: ReaderT ApiOptions m a -> ApiOptions -> m a
runWith actions state = runReaderT actions state



runWithAuthId :: ReaderT ApiOptions m a -> String -> m a
runWithAuthId actions string_id = runWith actions (defaultApiOptions { apiKey = Just $ BSC.pack string_id })



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



getAt :: By by => [(String, String)] -> [by] -> [String] -> ApiEff (Either Status ByteString)
getAt params' by' paths = do

  opts <- fixOpts params'
  url <- urlFromReader

  let url' = routeQueryBy url paths params' by'
  runDebug (log ("getAt: " <> url'))
  r <- liftIO $ getWith opts url'
  properResponse r



postAt :: (By by, WreqTypes.Postable a) => [(String, String)] -> [by] -> [String] -> a -> ApiEff (Either Status ByteString)
postAt params' by' paths body = do

  opts <- fixOpts params'
  url <- urlFromReader

  let url' = routeQueryBy url paths params' by'
  runDebug (log ("postAt: " <> url'))
  r <- liftIO $ postWith opts url' body
  properResponse r



putAt :: (By by, WreqTypes.Putable a) => [(String, String)] -> [by] -> [String] -> a -> ApiEff (Either Status ByteString)
putAt params' by' paths body = do

  opts <- fixOpts params'
  url <- urlFromReader

  let url' = routeQueryBy url paths params' by'
  runDebug (log ("putAt: " <> url'))
  r <- liftIO $ putWith opts url' body
  properResponse r



deleteAt :: By by => [(String, String)] -> [by] -> [String] -> ApiEff (Either Status ByteString)
deleteAt params' by' paths = do

  opts <- fixOpts params'
  url <- urlFromReader

  let url' = routeQueryBy url paths params' by'
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
