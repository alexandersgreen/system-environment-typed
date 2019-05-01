{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module System.Environment.Typed
    ( EnvVar(..)
    , ToEnvVar(..)
    , envVarList
    , FromEnvVar(..)
    , lookupEnvVars
    , mkEnvVarsRecord
    ) where

import System.Environment.Typed.Internal

envVarList :: (EnvVars args [(String,String)]) => p args -> EnvVarFunction args [(String,String)]
envVarList = flip envVarsFunction id 

type PATH = EnvVar "PATH" String
type PWD  = EnvVar "PWD"  String

type Environment = [ PATH , PWD ]

mkEnvVarsRecord ''Environment

lookupEnvironment :: IO EnvironmentRecord
lookupEnvironment = lookupEnvVars (Proxy :: Proxy Environment) EnvironmentRecord
