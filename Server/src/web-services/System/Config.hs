{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module System.Config (
    Config(..)
  , parseAndInitConfig
  , getClientPath
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
import qualified Data.Vector                  as Vector
import qualified Data.ByteString              as BS
import qualified Data.String                  as String
import qualified Data.HashMap.Strict          as HashMap


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



-- --------------
-- Local Scope
-- --------------
-- + Local
import qualified System.Config.Paths as Paths
-- ~



{-# ANN module ("HLint: ignore" :: Pre.String) #-}



-------------------------------------------------------------------------------
-- |



-------------------------------------------------------------------------------
-- | System Config Infrastructure
-------------------------------------------------------------------------------


data Config = Config
    { paths :: Paths.Paths
    }
    deriving (Show)


--
-- | Internal Helpers
--

configName :: Path Rel File
configName =
    [Path.relfile|system-config.yaml|]


findConfig :: IO (Path Abs File)
findConfig = do
    current <- PIO.getCurrentDir
    result <- PIO.findFile [current] configName
    
    case result of
        Nothing   -> error "Config file not found in current working directory."
        Just path -> return path


parseAndInitConfig :: IO Config
parseAndInitConfig = do
    filePath <- findConfig
    result <- Yaml.decodeFileEither (Path.toFilePath filePath) :: IO (Either Yaml.ParseException Yaml.Value)
    
    case result of
        Left err -> error $ Pre.unlines ["parseConfig", show err]
        Right yamlValue -> do
            initConfig filePath yamlValue


initConfig :: Path Abs File -> Yaml.Value -> IO Config
initConfig rootPath value = do
    paths <- Paths.initPaths rootPath value
    return Config
        { paths = paths
        }


-------------------------------------------------------------------------------
-- Config Fields
-------------------------------------------------------------------------------

parseWebClientPath :: Yaml.Value -> IO (Path Abs Dir)
parseWebClientPath (Yaml.Object xs) = do
    Path.parseAbsDir (Text.unpack rawClientPath)
    where
        (Just (Yaml.String rawClientPath)) = HashMap.lookup (Text.pack "web-client-path") xs



-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

getClientPath :: Config -> Path Abs Dir
getClientPath = paths >>> Paths.webClientPath_





