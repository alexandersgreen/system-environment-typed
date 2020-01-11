{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module Main where
  
import Data.Maybe (fromMaybe)
import System.Environment.Typed

type PATH = EnvVar "PATH" String
type PWD  = EnvVar "PWD"  String

type Environment = [ PATH , PWD ]

mkEnvVarsRecord ''Environment

lookupEnvironment :: IO EnvironmentRecord
lookupEnvironment = lookupEnvVars (Proxy :: Proxy Environment) EnvironmentRecord

main :: IO ()
main = do
  EnvironmentRecord{..} <- lookupEnvironment
  putStrLn $ "PATH=" ++ fromMaybe "<unset>" _PATH
  putStrLn $ "PWD="  ++ fromMaybe "<unset>" _PWD
