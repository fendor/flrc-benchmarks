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

type Iterations = Int
type Width = Int
type Height = Int

data Args = 
    FromFile FilePath Iterations
    | Generate Height Width Iterations

type CImage = Array U DIM2 Float

advance :: Int -> CImage -> IO CImage
advance !n image | n <= 0    = return image
                 | otherwise = compute image >>= advance (n - 1)

compute :: CImage -> IO CImage
compute image = Repa.computeP $ forStencil2 BoundClamp image
                     $ RepaStencil.makeStencil2 
                        3 
                        3
                        (\sh -> case sh of 
                            (Z :. -1 :. 0) -> Just 0.25
                            (Z :. 0 :. -1) -> Just 0.25
                            (Z :. 1 :. 0) -> Just 0.25
                            (Z :. 0 :. 1) -> Just 0.25
                            _ -> Nothing
                        )

buildIt :: [String] -> IO (IO CImage, Maybe (CImage -> IO ()))
buildIt args = image pArgs >>= \(img, iter) -> img `seq` return (runIt iter img, showIt)
  where
    pArgs = case args of
             []                  -> Generate 1024 1024 4000
             [iterinput]         -> Generate 1024 1024 (read iterinput)
             [li, wi, iterinput] -> Generate (read li) (read wi) (read iterinput)
             [fp, iterinput] -> FromFile fp (read iterinput)
             _                   -> error "2d-convolution.builtIt"

    runIt :: Iterations -> CImage -> IO CImage
    runIt = advance 

    image :: Args -> IO (CImage, Iterations)
    image arg = case arg of 
        FromFile filename iter  -> 
            flip (,) iter `fmap` parseMatrixFromFile filename

        Generate height width iters -> do
            let shape = (Z :. height :. width) :: DIM2
            return ((fromListUnboxed shape $ replicate (height * width) 1), iters)

    showIt :: Maybe (CImage -> IO ())
    showIt = Just (writeFile "2d-convolution.res" . show . sumAllS )    -- put checksum output in convolution.res

main :: IO ()
main = runBenchmark buildIt


parseMatrixFromFile :: FilePath -> IO CImage
parseMatrixFromFile filename = do
    content <- lines `fmap` readFile filename
    case content of 
      (sizes:values) -> do
          let [width, height] = (Prelude.map read . words) sizes
          let shape = Z :. height :. width :: DIM2
          return (fromListUnboxed shape $ concatMap (Prelude.map read . words) values)

      
      _ -> error "Couldn't parse file invalid number of lines "