{-# LANGUAGE NoImplicitPrelude #-}

module NbodyOptParser
  ( Options
  , iterationsOpt
  , numberOfPlanetsOpt
  , execOptionParser
  )
  where

import           ClassyPrelude
import           Data.Semigroup      ((<>))
import           Options.Applicative


data Options =
    Options
        { iterationsOpt      :: Int
        , numberOfPlanetsOpt :: Int
        }

execOptionParser :: IO Options
execOptionParser = execParser infos

infos :: ParserInfo Options
infos =
    info
        (options <**> helper)
        (  fullDesc
        <> progDesc "Nbody Problem Implementation"
        <> header "Simulate the NBody problem for a given number of times"
        )

options :: Parser Options
options =
    Options
        <$> option auto
            ( short 'n'
            <> long "iterations"
            <> help "Number of iterations execute the nbody problem"
            <> showDefault
            <> value 10
            <> metavar "INT"
            )
        <*> option auto
            ( short 's'
            <> long "planets"
            <> help "Number of planets to execute the nbody problem"
            <> showDefault
            <> value 12
            <> metavar "INT"
            )
