{-# LANGUAGE ForeignFunctionInterface #-}

module Pcm
    ( markerStart
    , markerEnd
    , markerInit
    , markerClose
    )
    where

import           Foreign
import           Foreign.C.Types
import Foreign.C.String

foreign import ccall safe "likwid_markerInit"
  c_MarkerInit :: IO ()

foreign import ccall safe "likwid_markerStartRegion"
  c_MarkerStart :: CString -> IO ()

foreign import ccall safe "likwid_markerStopRegion"
  c_MarkerEnd :: CString -> IO ()

foreign import ccall safe "likwid_markerClose"
  c_MarkerClose :: IO ()

markerStart :: String -> IO ()
markerStart marker = do
    c_marker <- newCString marker
    c_MarkerStart c_marker


markerEnd :: String -> IO ()
markerEnd marker = do
    c_marker <- newCString marker
    c_MarkerEnd c_marker

markerInit :: IO ()
markerInit = c_MarkerInit

markerClose :: IO ()
markerClose = c_MarkerClose