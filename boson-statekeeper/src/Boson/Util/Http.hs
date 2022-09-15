module Boson.Util.Http (isStatusCodeException) where

import qualified Network.HTTP.Client as L
import Network.HTTP.Req (HttpException (VanillaHttpException))
import RIO

isStatusCodeException :: HttpException -> Maybe (L.Response (), ByteString)
isStatusCodeException
  ( VanillaHttpException
      ( L.HttpExceptionRequest
          _
          (L.StatusCodeException r body)
        )
    ) = Just (r, body)
isStatusCodeException _ = Nothing
