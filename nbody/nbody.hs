{-
-- Intel Concurrent Collections for Haskell
-- Copyright (c) 2010, Intel Corporation.
--
-- This program is free software; you can redistribute it and/or modify it
-- under the terms and conditions of the GNU Lesser General Public License,
-- version 2.1, as published by the Free Software Foundation.
--
-- This program is distributed in the hope it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
-- more details.
--
-- You should have received a copy of the GNU Lesser General Public License along with
-- this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.

-- Ported from CnC/C++ program by Ryan Newton
-- Modified for use with Repa and HRC by Leaf Petersen (2012)
-- Refactored and further edited by Hannes Siebenhandl (2017)
-}

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Array.Repa as R
import           Data.List       (intercalate)
import qualified Data.List       as List
import           GHC.Conc        (numCapabilities)
import           Harness
import           NbodyOptParser
import qualified Text.Printf     as T

type Float3D  = (Double, Double, Double)
type PVector  = R.Array R.U R.DIM1 Float3D
type PVectorD = R.Array R.D R.DIM1 Float3D

gForce :: Double
{-# INLINE gForce #-}
gForce = 9.8

-- This step generates the bodies in the system.
genVector :: (R.Shape sh, Fractional t) => sh -> sh -> (t, t, t)
{-# INLINE genVector #-}
genVector sh tag = (tag' * 1.0, tag' * 0.2, tag' * 30.0)
   where tag' = fromIntegral (R.toIndex sh tag)

multTriple :: Double -> Float3D -> Float3D
{-# INLINE multTriple #-}
multTriple c (x, y, z) = ( c*x, c*y, c*z )

sumTriples :: PVectorD -> Float3D
{-# INLINE sumTriples #-}
sumTriples = R.foldAllS (\(x,y,z) (x',y',z') -> (x+x',y+y',z+z')) (0,0,0)

accel :: Float3D -> PVector -> Float3D
{-# INLINE accel #-}
accel vector vecList = multTriple gForce . sumTriples $ R.map (pairWiseAccel vector) vecList

pairWiseAccel :: Float3D -> Float3D -> Float3D
{-# INLINE pairWiseAccel #-}
pairWiseAccel (!x,!y,!z) (!x',!y',!z') =
  let
      dx = x'-x
      dy = y'-y
      dz = z'-z
      eps = 0.005
      distanceSq = dx*dx + dy*dy + dz*dz + eps
      factor = 1/sqrt(distanceSq * distanceSq * distanceSq)
  in
      multTriple factor (dx, dy, dz)

advance :: Monad m => Int -> PVector -> m PVector
{-# INLINE advance #-}
advance n accels =
    if n <= 0 then
      return accels
    else do
      next <- step accels
      advance (n-1) next

step :: Monad m => PVector -> m PVector
{-# INLINE step #-}
step accels = R.computeUnboxedP $ R.map (`accel` accels) accels

buildIt :: Monad m => Options -> m (m PVector, Maybe (PVector -> Integer -> IO ()))
buildIt options = do
    let d = R.ix1 planets
    vec <- R.computeUnboxedP (R.fromFunction d $ genVector d)
    return (runIt vec, showIt)
      where
        num = iterationsOpt options
        planets = numberOfPlanetsOpt options

        runIt :: Monad m => PVector -> m PVector
        {-# INLINE runIt #-}
        runIt vec = do
          acc <- advance num vec
          acc `R.deepSeqArray` return acc

        showIt :: Maybe (PVector -> Integer -> IO ())
        {-# INLINE showIt #-}
        showIt =
            let
                f r td = do
                    let line = intercalate "," [show numCapabilities, show planets, show num, show td]
                    appendFile "nbody.time.res" (line ++ "\n")
                    let s = List.concat [ T.printf "(%f, %f, %f)\n" x y z | (x, y, z) <- R.toList r ]
                    writeFile "nbody.res" s
            in
                Just f

main :: IO ()
main = runBenchmark 10 . buildIt =<< execOptionParser

prettyPrint :: PVector -> IO ()
prettyPrint = mapM_ print . R.toList
