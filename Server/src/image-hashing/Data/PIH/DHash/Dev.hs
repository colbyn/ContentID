{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE RebindableSyntax #-}
module Data.PIH.DHash.Dev where

-- ~
import Core hiding ((>=))
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
    )

import Data.Array.Accelerate ((:.)(..), Z(..), (>=))

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

img1Path = rootSamplePath ++ "/sample_01_large.jpg"
img2Path = rootSamplePath ++ "/sample_01_small.jpg"

imgRefPath = "/Users/colbyn/Desktop/page42image1773376.png"


img1OutPath =
    rootOutputPath ++ "/x1.jpg"

-- type InputFormat = [[Double]]


-- alpha =
--     let
--         mat = A.fromList (Z :. 9 :. 8) (List.repeat 0) :: A.Matrix Int
--     in
--         mat




-- run = do
--     (C.CTime start) <- Time.epochTime
--     img1 <- (serialize . preprocess) <$> Hip.readImageY Hip.VU img1Path
-- 
--     let x = GPU.run (process $ A.use img1)
-- 
--     let y = unserialize x
-- 
--     (C.CTime end) <- Time.epochTime
-- 
--     putStrLn
--         $ "Time: " ++ show (end - start)
--         ++ "\n"
-- 
--     return ()

-- 
-- run = do
--     img <- preprocess <$> Hip.readImageY Hip.VU imgRefPath
-- 
--     Hip.displayImage (Hip.pixelGrid 30 img)
-- 
--     return ()
-- 
-- 
-- 
-- preprocess :: Hip.Image Hip.VU Hip.Y Double -> Hip.Image Hip.VU Hip.Y Double
-- preprocess input =
--     Hip.resize Hip.Nearest Hip.Edge (9, 8) input
-- 
-- 
-- serialize :: Hip.Image Hip.VU Hip.Y Double -> A.Matrix Double
-- serialize =
--     pack' . extract
-- 
--     where
--         extract :: Hip.Image Hip.VU Hip.Y Double -> [[Double]]
--         extract = (map (map extract')) . Hip.toLists
-- 
--         extract' :: Hip.Pixel Hip.Y Double -> Double
--         extract' (Hip.PixelY x) = x
-- 
-- 
--         pack' :: [[Double]] -> A.Matrix Double
--         pack' = A.fromList (Z :. 9 :. 8) . flatten
-- 
-- 
-- 
-- 
-- unserialize :: A.Matrix Double -> [Double]
-- unserialize =
--     A.toList
-- 
-- 
-- 
-- diff :: A.Stencil3x3 Double -> A.Exp Double
-- diff
--     ((_, _, _)
--     ,(_, c, _)
--     ,(_, b, _)) =
-- 
--         A.cond (c >= b) 1 0
-- 
-- 
-- process :: GPU.Acc (A.Array A.DIM2 Double) -> GPU.Acc (A.Array A.DIM2 Double)
-- process input =
--     A.stencil diff A.clamp input






