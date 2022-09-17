module Gluon.Util.Exception (EmptyMaybeException (..), fromJustOrThrow) where

import RIO

newtype EmptyMaybeException = EmptyMaybeException Text
  deriving (Show)

instance Exception EmptyMaybeException

fromJustOrThrow :: Text -> Maybe a -> RIO env a
fromJustOrThrow _ (Just a) = pure a
fromJustOrThrow msg Nothing = throwM $ EmptyMaybeException msg
