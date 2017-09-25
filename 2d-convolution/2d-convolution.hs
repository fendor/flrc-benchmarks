{-
 - Redistribution and use in source and binary forms, with or without modification, are permitted
 - provided that the following conditions are met:
 - 1.   Redistributions of source code must retain the above copyright notice, this list of
 - conditions and the following disclaimer.
 - 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 - conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 - THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 - BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 - ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 - EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 - OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 - OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 - IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import           ConvolutionOptParser
import           Data.Array.Repa                     ((:.) (..), Array, DIM2, U,
                                                      Z (Z), computeP, extent,
                                                      fromListUnboxed, sumAllS)
import qualified Data.Array.Repa                     as Repa
import           Data.Array.Repa.Algorithms.Convolve (convolveOutP, outClamp)
import           Data.Array.Repa.Stencil             (Boundary (BoundClamp),
                                                      Stencil)
import           Data.Array.Repa.Stencil.Dim2        (forStencil2, makeStencil2)
import           Data.List                           (intercalate)
import           GHC.Conc                            (numCapabilities)
import           Harness

type CImage = Array U DIM2 Double

newtype CStencil =
    CStencil {-# UNPACK #-} (Array U DIM2 Double)



defaultStencil :: Stencil DIM2 Double
{-# INLINE defaultStencil #-}
defaultStencil =
    makeStencil2
        3
        3
        (\sh -> case sh of
            (Z :. 0 :. 0)  -> Just 0.5
            (Z :. 0 :. 1)  -> Just 0.125
            (Z :. 1 :. 0)  -> Just 0.125
            (Z :. -1 :. 0) -> Just 0.125
            (Z :. 0 :. -1) -> Just 0.125
            _              -> Nothing
        )

compute :: CImage -> IO CImage
{-# INLINE compute #-}
compute image =
    computeP $ forStencil2 BoundClamp image defaultStencil

compute' :: CImage -> CStencil -> IO CImage
{-# INLINE compute' #-}
compute' img (CStencil stencil) =
    convolveOutP outClamp stencil img

advance' :: Int -> CImage -> CStencil -> IO CImage
{-# INLINE advance' #-}
advance' !n image stencil
    | n <= 0  = return image
    | otherwise = compute' image stencil >>= \img -> advance' (n-1) img stencil

advance :: Int -> CImage -> IO CImage
{-# INLINE advance #-}
advance !n image
    | n <= 0    = return image
    | otherwise = compute image >>= \img -> advance (n - 1) img

{--}
buildIt :: Options -> IO (IO CImage, Maybe (CImage -> Integer -> IO ()))
buildIt options = do
    writeFile "2d-convolution.res" "" -- truncate output file
    img <- getImage (imageOpt options)

    case stencilOpt options of
        FileStencil file -> do
            stencil <- CStencil `fmap`  parseMatrixFromFile file
            img `seq` return (runIt' img stencil, showIt $ extent img)

        Default ->
            img `seq` return (runIt img , showIt $ extent img)

    where
        num = iterationsOpt options

        runIt :: CImage -> IO CImage
        {-# INLINE runIt #-}
        runIt = advance num

        runIt' :: CImage -> CStencil -> IO CImage
        {-# INLINE runIt' #-}
        runIt' = advance' num

        getImage :: OptImage -> IO CImage
        getImage (FileImage file)      = parseMatrixFromFile file
        getImage (Extent width height) = computeP . Repa.fromFunction (Z:.height:.width) $ const 1

        showIt :: DIM2 -> Maybe (CImage -> Integer -> IO ())
        showIt (Z :. height :. width) =
            Just foo
            where
              foo :: CImage -> Integer -> IO ()
              foo k td = do
                  appendFile "2d-convolution.time.res" $ intercalate "," [show numCapabilities, show height, show width, show num, show td] ++ "\n"
                  appendFile "2d-convolution.res" $ show (sumAllS k) ++ "\n"
--}
main :: IO ()
main = runBenchmark 10 . buildIt =<< execOptionParser


parseMatrixFromFile :: FilePath -> IO CImage
parseMatrixFromFile filename = do
    content <- lines `fmap` readFile filename
    case content of
      (sizes:values) -> do
          let [width, height] = (Prelude.map read . words) sizes
          let shape = Z :. height :. width :: DIM2
          return (fromListUnboxed shape $ concatMap (Prelude.map read . words) values)


      _ -> error "Couldn't parse file invalid number of lines "
