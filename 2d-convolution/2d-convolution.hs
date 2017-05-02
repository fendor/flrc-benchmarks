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
{-# LANGUAGE QuasiQuotes           #-}
import           Data.Array.Repa              as Repa
import           Data.Array.Repa.Stencil      as R (Boundary (BoundClamp))
import           Data.Array.Repa.Stencil.Dim2 as RepaStencil
import           Harness

type CImage = Array U DIM2 Float

advance :: Int -> CImage -> IO CImage
advance !n image | n <= 0    = return image
                 | otherwise = compute image >>= advance (n - 1)

compute :: CImage -> IO CImage
compute image = Repa.computeP $ forStencil2 BoundClamp image
                     [stencil2|   2 -2  2 -2  2
                                 -2  2 -2  2 -2
                                  2 -2 -1 -2  2
                                 -2  2 -2  2 -2
                                  2 -2  2 -2  2 |]

buildIt :: Monad m => [String] -> m (IO CImage, Maybe (CImage -> IO ()))
buildIt args = image `seq` return (runIt, showIt)
  where
    (iterations, ilength, iwidth) = case args of
             []                  -> (4000, 1024, 1024)
             [iterinput]         -> (read iterinput, 1024, 1024)
             [iterinput, li]     -> (read iterinput, read li, 1024)
             [iterinput, li, wi] -> (read iterinput, read li, read wi)
             _                   -> error "2d-convolution.builtIt"

    runIt :: IO CImage
    runIt = advance iterations image

    shape :: DIM2
    shape = Z :. ilength :. iwidth

    image :: CImage
    image = Repa.computeS . fromFunction shape $ const 1

    showIt :: Maybe (CImage -> IO ())
    showIt = Just (writeFile "2d-convolution.res" . show . sumAllS)    -- put checksum output in convolution.res

main :: IO ()
main = runBenchmark buildIt
