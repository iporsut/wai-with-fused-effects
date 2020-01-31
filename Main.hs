{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict
import Control.Carrier.Lift
import Control.Effect.Class
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Streaming as Streaming
import Streaming.Prelude ( Of( (:>) ) )

type ByteStream = Streaming.ByteString IO

type Web sig m = ( Has (Reader Wai.Request) sig m
                 , Has (State HTTP.Status) sig m
                 , Has (Writer HTTP.ResponseHeaders) sig m
                 , Has (Lift ByteStream) sig m
                 )

helloWorld :: Web sig m => m ()
helloWorld = do
  req <- ask @Wai.Request
  tell @HTTP.ResponseHeaders [(HTTP.hContentType, "text/plain")]
  sendM @ByteStream "Hello, world!\n"
  sendM @ByteStream ("You requested " <> Streaming.fromStrict (Wai.rawQueryString req))
  put @HTTP.Status HTTP.ok200

type Application = StateC HTTP.Status (WriterC HTTP.ResponseHeaders (ReaderC Wai.Request (LiftC ByteStream)))

runApplication :: Application () -> Wai.Application
runApplication action req respond = do
  result <-
    Streaming.toLazy
    . runM @ByteStream
    . runReader @Wai.Request req
    . runWriter @HTTP.ResponseHeaders
    . runState @HTTP.Status HTTP.status500
    $ action
  let (respBody :> (headers, (status, ()))) = result
  respond (Wai.responseLBS status headers respBody)

main :: IO ()
main = Warp.run 8080 (runApplication helloWorld)
