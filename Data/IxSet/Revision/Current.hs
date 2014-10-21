{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TemplateHaskell,
             UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans -Wwarn #-}
module Data.IxSet.Revision.Current
    ( Ident(..)
    , Revision(..)
    , prettyRevision
    , RevisionInfo(..)
    , prettyRevisionInfo
    , NodeStatus(..)
    ) where

import Data.Generics
import Data.Int (Int64)
import Text.PrettyPrint (Doc, text)

type EpochMilli = Int64

-- | Identifier for a item which can have multiple revisions.  This is
-- an example of a type that could be used as the @k@ type of
-- @RevisionInfo k@.
newtype Ident
    = Ident {unIdent :: Integer}
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Enum Ident where
    toEnum = Ident . toInteger
    fromEnum = fromInteger . unIdent

-- | Each node of the revision graph is either a @Head@, which means
-- it is a "current" revision and not the parent of any other node, or
-- @NonHead@ which means some other revision is newer.
data NodeStatus = Head | NonHead deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | 'RevisionInfo' holds all the information associated with a
-- | particular revision of a value.
data Enum k => RevisionInfo k
    = RevisionInfo
      { revision :: Revision k -- ^ Contains the value identifier and the sequence number of the revision.
      , created :: EpochMilli  -- ^ The time at which the revision was created
      , parentRevisions :: [Integer] -- ^ The revision numbers from which this revision was derived.
      , nodeStatus :: NodeStatus -- ^ A revision has status NonHead if it is the parent of any other revision
      } deriving (Eq, Ord, Read, Data, Typeable)

-- | 'Revision' Identifier for a particular revision of a particular item.
data Enum k => Revision k
    = Revision
      { ident :: k        -- ^ Identifier common to all the revisions of this value.
      , number :: Integer -- ^ A sequence number assigned to this particular revision
      }
    deriving (Eq, Ord, Read, Data, Typeable)

prettyRevisionInfo :: (Show k, Enum k) => RevisionInfo k -> Doc
prettyRevisionInfo r =
    text ("(" ++ show (prettyRevision (revision r)) ++
          " created: " ++ show (created r) ++
          (if nodeStatus r == Head then " (Head)" else " (NonHead)") ++
          " parents: " ++ show (parentRevisions r) ++ ")")

prettyRevision :: (Show k, Enum k) => Revision k -> Doc
prettyRevision r = text (show (ident r) ++ "." ++ show (number r))
