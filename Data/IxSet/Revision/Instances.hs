{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.IxSet.Revision.Instances where

import Data.Generics
import Data.SafeCopy (base, deriveSafeCopy)
import Happstack.Data (Version(..), Proxy(..), Mode(..), proxy, extension, deriveSerialize, Migrate(..))
import qualified Data.IxSet.Revision.Old1 as O1
import qualified Data.IxSet.Revision.Current as C


---------------
-- MIGRATION --
---------------

-- |Obsolete version of the RevisionInfo structure.  The isHead field
-- was changed to nodeStatus :: NodeStatus.
data RevisionInfo001
    = RevisionInfo001
      { revision001 :: O1.Revision002
      , parentRevisions001 :: [Integer]
      , isHead001 :: Bool }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Enum k => Migrate O1.RevisionInfo002 (C.RevisionInfo k) where
    migrate r =
        C.RevisionInfo
             { C.revision = migrate (O1.revision002 r)
             , C.created = O1.created002 r
             , C.parentRevisions = O1.parentRevisions002 r
             , C.nodeStatus = O1.nodeStatus002 r }

instance Enum k => Migrate O1.Revision002 (C.Revision k) where
    migrate r =
        C.Revision
             { C.ident = toEnum $ fromEnum $ O1.ident002 r
             , C.number = O1.number002 r }

instance Version O1.Revision002
$(deriveSerialize ''O1.Revision002)
$(deriveSafeCopy 1 'base ''O1.Revision002)
instance Version O1.RevisionInfo002
$(deriveSerialize ''O1.RevisionInfo002)
$(deriveSafeCopy 1 'base ''O1.RevisionInfo002)
instance Enum k => Version (C.RevisionInfo k) where
    mode = x
        where
          x :: Enum k => Mode (C.RevisionInfo k)
          x = extension 2 y
          y :: Proxy O1.RevisionInfo002
          y = proxy undefined
$(deriveSerialize ''C.RevisionInfo)
$(deriveSafeCopy 1 'base ''C.RevisionInfo)
instance Enum k => Version (C.Revision k) where
    mode = x
        where
          x :: Enum k => Mode (C.Revision k)
          x = extension 1 (proxy undefined :: Proxy O1.Revision002)
$(deriveSerialize ''C.Revision)
$(deriveSafeCopy 1 'base ''C.Revision)
instance Version C.NodeStatus
$(deriveSerialize ''C.NodeStatus)
$(deriveSafeCopy 1 'base ''C.NodeStatus)
instance Version C.Ident
$(deriveSerialize ''C.Ident)
$(deriveSafeCopy 1 'base ''C.Ident)
