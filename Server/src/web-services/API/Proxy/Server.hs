{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module API.Proxy.Server (
    initRootAPIs
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
-- ~



{-# ANN module ("HLint: ignore" :: Pre.String) #-}







-------------------------------------------------------------------------------
-- |



initRootAPIs :: Config -> IO W.Application
initRootAPIs config = return rootAPIApp


rootAPIApp :: W.Application
rootAPIApp req res = res $ W.responseLBS Status.ok200 [] "Hello World..."



