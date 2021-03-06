{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Proxy.Server (
    startRootServer
) where


-- ~
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid       ((<>), Monoid)
import Control.Arrow     ((<<<), (>>>))
import Prelude
    ( return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , putStrLn
    , fromIntegral
    )

import qualified Prelude    as Pre
import qualified Core.Utils as Core

import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M
import qualified Control.Foldl              as ML

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.String                  as String


-- + C FFI
import qualified Foreign.C.Types as C

-- + OS APIS & Related
import Path ((</>), Path, Abs, Rel, Dir, File)

import qualified Path
import qualified Path.IO               as PIO
import qualified System.Directory      as SD
import qualified System.FilePath.Posix as FP
import qualified System.Posix.Time     as Time
import qualified System.IO.Temp        as Temp
import qualified GHC.IO.Handle         as Handle
import qualified System.Process         as SP

-- + Concurrency & Related
import qualified Control.Concurrent       as CC
import qualified Control.Concurrent.Async as Async

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP 

-- + Yaml
import qualified Data.Yaml.Parser as Parser
import qualified Data.Yaml        as Yaml


---------------------
-- + Image Processing
---------------------
import Codec.Picture as Picture


-- --------------
-- WAI & Warp (LL)
-- --------------
-- + Helpers
import qualified Data.Text.Encoding as Encoding

-- + Base
import qualified Network.Wai                     as W
import qualified Network.Wai.Handler.Warp        as Warp
import qualified Network.HTTP.Types.Status       as Status
import qualified Network.HTTP.Types.Header       as Header
import qualified WaiAppStatic.Types              as WS
import qualified WaiAppStatic.Storage.Filesystem as FS
import qualified WaiAppStatic.Listing            as Listing
import qualified Network.Wai.Application.Static  as Static
import qualified Network.Wai.Middleware.Rewrite  as Rewrite

-- HTTP Proxy Stuff
import qualified Network.HTTP.ReverseProxy as Proxy

-- + Dev/Debug
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger


-- -------------
-- Servant
-- -------------
import Servant.API
    ( (:>)
    , (:<|>)
    )

import qualified Servant.API as API



-- --------------
-- Local Scope
-- --------------
-- + Local Deps
import System.Config (Config)

import qualified System.Config.Paths as Paths
import qualified System.Config       as Config

-- + Local
import qualified Frontend.WebClient.Server as WebClient
import qualified API.Proxy.Server
-- ~


{-# ANN module ("HLint: ignore" :: Pre.String) #-}



-------------------------------------------------------------------------------
-- |

type FileServerApp = W.Application
type APIServerApp = W.Application

rootServerPort = 3000


startRootServer :: Config.Config -> IO ()
startRootServer config = do
    PP.prettyPrint "Server: Starting Root Server."
    PP.prettyPrint $ "Server: Port: " ++ show rootServerPort
    
    -- Init Applications
    fileServerApp <- WebClient.initFileServerApp config
    apiServers <- API.Proxy.Server.initRootAPIs config
    
    -- Root Application
    let rootApp = proxySwitch fileServerApp apiServers
    
    -- Exec
    Warp.run rootServerPort (RequestLogger.logStdoutDev rootApp)


proxySwitch :: FileServerApp -> APIServerApp -> W.Application
-- File Server Cases
proxySwitch fs _ req@(onDistPath -> True) res = fs (proxyDistPath req) res
proxySwitch fs _ req@(onIndexPath -> True) res = fs (proxyDistPath req) res
-- API Server Cases
proxySwitch _ as req@(onApiPath -> True) res = as (proxyApiPath req) res
-- Default Case - 404
proxySwitch _ _ req res = WebClient.notFoundApp req res



onDistPath :: W.Request -> Bool
onDistPath (W.pathInfo -> ("dist":rest)) = True
onDistPath _ = False

onIndexPath :: W.Request -> Bool
onIndexPath (W.pathInfo -> []) = True
onIndexPath (W.pathInfo -> [""]) = True
onIndexPath (W.pathInfo -> ["index"]) = True
onIndexPath (W.pathInfo -> ["index.html"]) = True
onIndexPath _ = False


onApiPath :: W.Request -> Bool
onApiPath (W.pathInfo -> ("api":rest)) = True
onApiPath _ = False


proxyDistPath :: W.Request -> W.Request
proxyDistPath = Rewrite.rewriteRequestPure f
    where
        f :: Rewrite.PathsAndQueries -> Header.RequestHeaders -> Rewrite.PathsAndQueries
        f ("dist":rest, qs) _ = (rest, qs)
        f x _ = x


proxyApiPath :: W.Request -> W.Request
proxyApiPath = Rewrite.rewriteRequestPure f
    where
        f :: Rewrite.PathsAndQueries -> Header.RequestHeaders -> Rewrite.PathsAndQueries
        f ("api":"v0":rest, qs) _ = (rest, qs)
        f ("api":rest, qs) _ = (rest, qs)


