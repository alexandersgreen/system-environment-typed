{-# LANGUAGE FlexibleContexts #-}

module System.Environment.Typed
    ( EnvVar(..)
    , ToEnvVar(..)
    , envVarList
    , FromEnvVar(..)
    , lookupEnvVars
    , mkEnvVarsRecord
    , Proxy(..)
    ) where

import System.Environment.Typed.Internal

envVarList :: (EnvVars args [(String,String)]) => p args -> EnvVarFunction args [(String,String)]
envVarList = flip envVarsFunction id 
