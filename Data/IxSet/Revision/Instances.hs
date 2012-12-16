{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.IxSet.Revision.Instances where

import Data.Generics
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.IxSet.Revision.Current as C

$(deriveSafeCopy 1 'base ''C.RevisionInfo)
$(deriveSafeCopy 1 'base ''C.Revision)
$(deriveSafeCopy 1 'base ''C.NodeStatus)
$(deriveSafeCopy 1 'base ''C.Ident)
