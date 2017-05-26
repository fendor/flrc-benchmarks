{-# LANGUAGE NoImplicitPrelude #-}

module ConvolutionOptParser
  (execOptionParser
  , Options
  , iterationsOpt
  , imageOpt
  , stencilOpt
  , OptStencil(..)
  , OptImage(..)
  , Width
  , Height
  , Iterations
  ) where

import           ClassyPrelude
import           Data.Semigroup      ((<>))
import           Options.Applicative

type Width = Int
type Height = Int
type Iterations = Int

data OptStencil
    = FileStencil FilePath
    | Default
    deriving (Show, Eq, Read)

data OptImage
    = FileImage FilePath
    | Extent Width Height
    deriving (Show, Eq, Read)

data Options =
    Options
        { iterationsOpt :: Int
        , imageOpt      :: OptImage
        , stencilOpt    :: OptStencil
        }
        deriving (Show, Eq, Read)

execOptionParser :: IO Options
execOptionParser = execParser infos

infos :: ParserInfo Options
infos =
    info
        (options <**> helper)
        (  fullDesc
        <> progDesc "2D - Convolution Implementation"
        <> header "2D - Convolution: apply a stencil to an image"
        )


options :: Parser Options
options =
    Options
      <$> option auto
          (  short 'n'
          <> long "iterations"
          <> help "Number of iterations to apply the stencil to the image"
          <> showDefault
          <> value 400
          <> metavar "INT"
          )
      <*> (imageWithExtentParser <|> imageFromFileParser)
      <*> (stencilFromFileParser <|> stencilDefaultParser)

imageFromFileParser :: Parser OptImage
imageFromFileParser =
    FileImage
        <$> strOption
            ( short 'f'
            <> long "image"
            <> help "Filepath to the image"
            )

imageWithExtentParser :: Parser OptImage
imageWithExtentParser =
    Extent
        <$> option auto
            (  short 'w'
            <> long "width"
            <> help "Width of the image to generate"
            <> showDefault
            <> value 512
            <> metavar "INT"
            )
        <*> option auto
            (  short 'h'
            <> long "height"
            <> help "Height of the image to generate"
            <> showDefault
            <> value 512
            <> metavar "INT"
            )

stencilFromFileParser :: Parser OptStencil
stencilFromFileParser =
    FileStencil
        <$> strOption
            ( short 'k'
            <> long "kernel"
            <> help "Filepath to stencil file"
            )

stencilDefaultParser :: Parser OptStencil
stencilDefaultParser =
    pure Default
