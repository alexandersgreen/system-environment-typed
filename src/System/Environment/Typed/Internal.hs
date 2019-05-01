{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module System.Environment.Typed.Internal
 ( module System.Environment.Typed.Internal
 , Proxy(..)
 ) where

import Data.Proxy   (Proxy(..))
import GHC.TypeLits (Symbol,KnownSymbol,symbolVal)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Environment (lookupEnv)

data EnvVar (name :: Symbol) (typ :: *) :: *

class MkEnvVar a where
  mkEnvVar :: a -> (String,String)

class ToEnvVar a where
  toEnvVar :: a -> String

instance ToEnvVar String where
  toEnvVar = id

type family EnvVarType (envVar :: *) :: * where
  EnvVarType (EnvVar name typ) = typ

newtype EnvVarValue (envVar :: *) = EnvVarValue { envVarValue :: EnvVarType envVar }

instance (KnownSymbol name,ToEnvVar typ) => MkEnvVar (EnvVarValue (EnvVar name typ)) where
  mkEnvVar EnvVarValue{..} = ( symbolVal (Proxy :: Proxy name)
                             , toEnvVar  envVarValue
                             ) 

type family EnvVarFunction (args :: [*]) (result :: *) :: * where
  EnvVarFunction '[]      result = result
  EnvVarFunction (a : as) result = EnvVarType a -> EnvVarFunction as result

class EnvVars (args :: [*]) (result :: *) where
  envVarsFunction :: p args -> ([(String,String)] -> result) -> EnvVarFunction args result

instance EnvVars '[] result where
  envVarsFunction _ f = f []

instance (MkEnvVar (EnvVarValue (EnvVar name typ)), EnvVars as result) => EnvVars ((EnvVar name typ) : as) result where
  envVarsFunction _ f val = envVarsFunction (Proxy :: Proxy as) (f . (:) (mkEnvVar (EnvVarValue val :: EnvVarValue (EnvVar name typ))))

class LookupEnvVar a where
  lookupEnvVar :: IO (Maybe a)

class FromEnvVar a where
  fromEnvVar :: String -> Maybe a

instance FromEnvVar String where
  fromEnvVar = Just

instance (KnownSymbol name, FromEnvVar typ) => LookupEnvVar (EnvVarValue (EnvVar name typ)) where
  lookupEnvVar = do
    mEnvVar <- lookupEnv $ symbolVal (Proxy :: Proxy name)
    pure $ fmap EnvVarValue $ mEnvVar >>= fromEnvVar

type family EnvVarLookup (args :: [*]) (result :: *) :: * where
  EnvVarLookup '[]      result = result
  EnvVarLookup (a : as) result = Maybe (EnvVarType a) -> EnvVarLookup as result

class LookupEnvVars (args :: [*]) (result :: *) where
  lookupEnvVars :: p args -> EnvVarLookup args result -> IO result

instance LookupEnvVars '[] result where
  lookupEnvVars _ result = pure result

instance (LookupEnvVar (EnvVarValue a), LookupEnvVars as result) => LookupEnvVars (a : as) result where
  lookupEnvVars _ f = do
    a :: Maybe (EnvVarValue a) <- lookupEnvVar
    lookupEnvVars (Proxy :: Proxy as) (f (envVarValue <$> a)) 

mkEnvVarsRecord :: Name ->  Q [Dec]
mkEnvVarsRecord name = do
  typ     <- getTyp name
  names   <- getNames typ
  envVars <- flip mapM names $ \name -> do
    typ <- getTyp name
    getEnvVar typ
  let recordName = mkName $ nameBase name <> "Record"
      fields = map mkField envVars
  pure [DataD [] recordName [] Nothing [RecC recordName fields] [DerivClause Nothing [ConT ''Show]]]
    where
      getTyp :: Name -> Q Type
      getTyp name = do
        inf <- reify name
        case inf of
          (TyConI (TySynD _ [] typ)) -> pure typ
          _ -> fail "mkEnvVarsRecord: Expecting a type-synonym"

      getNames :: Type -> Q [Name]
      getNames (AppT (AppT PromotedConsT (ConT nm)) rest)
        = getNames rest >>= pure . (:) nm
      getNames (SigT PromotedNilT (AppT ListT StarT)) 
        = pure []
      getNames t = fail $ "mkEnvVarsRecord: Expecting a type level list, but got: " ++ show t

      getEnvVar :: Type -> Q (String,Type)
      getEnvVar (AppT (AppT (ConT constructor) (LitT (StrTyLit name))) typ)
        | constructor == ''EnvVar = pure (name,typ)
      getEnvVar _ = fail "mkEnvVarsRecord: Expecting an EnvVar"

      mkField :: (String,Type) -> VarBangType
      mkField (name,typ) = 
       ( mkName $ "_" ++ name
       , Bang NoSourceUnpackedness NoSourceStrictness
       , AppT (ConT ''Maybe) typ
       )

