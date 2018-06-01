{-# LANGUAGE NoImplicitPrelude #-}
module Component.DHash.Dev where


-- ~
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    (return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    , (/)
    )

import qualified Prelude as Pre

-- import Data.Array.Accelerate ((:.)(..), Z(..), (>=))

import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

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


-- + C FFI
import qualified Foreign.C.Types as C

-- + OS APIS & Related
import qualified Path
import qualified System.Directory      as SD
import qualified System.FilePath.Posix as FP
import qualified System.Posix.Time     as Time
import qualified System.Process        as SP

-- + Concurrency & Related
import qualified Control.Concurrent       as CC
import qualified Control.Concurrent.Async as Async

-- + JuicyPixels & Related
import qualified Codec.Picture as P


-- + HIP & Related
import qualified Graphics.Image            as Hip
import qualified Graphics.Image.Interface  as Hip
import qualified Graphics.Image.Processing as Hip

-- + Accelerate & Related
-- import qualified Data.Array.Accelerate          as A
-- import qualified Data.Array.Accelerate.LLVM.PTX as GPU

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP
-- ~



{-# ANN module "HLint: ignore" #-}





rootOutputPath = "/Users/colbyn/Developer/SubSystems/TV/Indexer/testdata/output"
rootSamplePath = "/Users/colbyn/Developer/SubSystems/TV/Indexer/testdata/set1"

img1Path = rootSamplePath ++ "/sample_04_small.jpg"
img2Path = rootSamplePath ++ "/sample_04_large.jpg"


imgRefPath = "/Users/colbyn/Desktop/page42image1773376.png"





beta = do
    img <- preprocess <$> Hip.readImageY Hip.VU img1Path
    
    -- Hip.displayImage (Hip.pixelGrid 100 img)
    Hip.displayImage img
    
    return ()



preprocess :: Hip.Image Hip.VU Hip.Y Double -> Hip.Image Hip.VU Hip.Y Double
preprocess input =
    Hip.resize Hip.Nearest Hip.Edge (50, 50) input


-- resize :: Hip.Image Hip.VU Hip.Y Double -> Hip.Image Hip.VU Hip.Y Double
-- resize img =
--     Hip.traverse img (const (newHeight, newWidth)) getNewPx
--     where
--         newHeight = 50
--         newWidth = 50
-- 
--         size@(height, width) = Hip.dims img
-- 
--         heightRatio = fromIntegral newHeight / fromIntegral height
--         widthRatio = fromIntegral newWidth / fromIntegral width
-- 
--         getNewPx getter (i, j) =
--             Hip.interpolate
--                 Hip.Nearest
--                 Hip.Edge
--                 size
--                 getter
--                 ((fromIntegral i + 0.5) / heightRatio - 0.5, (fromIntegral j + 0.5) / widthRatio - 0.5)



-- resize' :: Hip.Image Hip.VU Hip.Y Double -> Hip.Image Hip.VU Hip.Y Double
-- resize' img =
--     Hip.traverse img (const (newHeight, newWidth)) getNewPx
--     where
--         newHeight = 50
--         newWidth = 50
-- 
--         size@(sourceHeight, sourceWidth) = Hip.dims img
-- 
--         heightRatio = fromIntegral sourceHeight / fromIntegral newHeight
--         widthRatio = fromIntegral sourceWidth / fromIntegral newWidth
-- 
--         line y = Pre.round
--             $ (double y + 0.5) * heightRatio - 0.5
-- 
--         col x = Pre.round
--             $ (double x + 0.5) * widthRatio - 0.5
-- 
--         getNewPx getter (i, j) =
--             Hip.interpolate
--                 Hip.Nearest
--                 Hip.Edge
--                 size
--                 getter
--                 ((fromIntegral i + 0.5) / heightRatio - 0.5, (fromIntegral j + 0.5) / widthRatio - 0.5)



alpha = do
    x <- Hip.readImageY Hip.VU img1Path
    
    let downsample' = downsample x
    
    let resImg = Hip.imap downsample' x
    
    
    Hip.displayImage resImg
    
    return ()


downsample :: Hip.Image Hip.VU Hip.Y Double -> (Int, Int) -> Hip.Pixel Hip.Y Double -> Hip.Pixel Hip.Y Double
downsample img (y, x) p =
    Hip.index img
        ( clamp 0 (oldHeight - 1) (inputY y)
        , clamp 0 (oldWidth - 1) (inputX x)
        )
    
    where
        size@(oldHeight, oldWidth) = Hip.dims img
        
        newHeight = 50
        newWidth  = 50

        heightRatio = double oldHeight / double newHeight
        widthRatio  = double oldWidth  / double newWidth
        
        -- heightRatio = double newHeight / double oldHeight
        -- widthRatio  = double newWidth / double oldWidth
        
        inputY y = Pre.truncate
            $ (double y + 0.5) * heightRatio - 0.5
        
        inputX x = Pre.truncate
            $ (double x + 0.5) * widthRatio - 0.5














clamp min' max' x
    | x < min'  = min'
    | x > max'  = max'
    | otherwise = x












double :: Pre.Integral a => a -> Double
double = fromIntegral

















