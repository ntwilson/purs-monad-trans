module Main where

import Prelude

import Data.Either (Either(..))
import Data.Interpolate (i)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Class.Console (log, logShow)

type Result = Int
type Response = Int

type ErrorMessage = String

newtype EffectEither a b = EffectEither (Effect (Either a b))
derive instance newtypeEffectEither :: Newtype (EffectEither a b) _
instance monadEffectEither :: Monad (EffectEither a) 

instance applicativeEffectEither :: Applicative (EffectEither err) where 
  -- let pure (x:'a) : IOResult<'a, 'err>) = 
  --   IOResult <| io { return result { return x } } 
  pure :: forall a. a -> EffectEither err a
  pure x = EffectEither $ pure $ pure x 

instance bindEffectEither :: Bind (EffectEither err) where
  bind :: forall a b. EffectEither err a -> (a -> EffectEither err b) -> EffectEither err b
  bind (EffectEither (x :: Effect (Either err _))) fn = EffectEither $ do
    (x' :: Either err _) <- x
    case x' of
      Left e -> pure $ Left e
      Right x'' -> 
        let (EffectEither (y :: Effect (Either err _))) = fn x''
        in y

  -- let bind (EffectEither (x : IO<Result<'a, 'err>>)) (fn : 'a -> IOResult<'b, 'err>) : IOResult<'b, 'err> = IOResult <| io {
  --   let! (x' : Result<'a, 'err>) = x
  --   match x' with
  --   | Error e -> return Error e
  --   | Ok x'' ->
  --     let (EffectEither (y : IO<Result<'b, 'err>>)) = fn x''
  --     return! y
  -- }

instance applyEffectEither :: Apply (EffectEither a) where apply = ap
derive instance functorEffectEither :: Functor (EffectEither a) 

ajaxRequest :: Effect (Either ErrorMessage Response)
ajaxRequest = pure $ pure 5
-- let ajaxRequest = io { return result { return 5 } }
-- let ajaxRequest = io { return Ok 5 }

processResponse :: Response -> Either ErrorMessage Result
processResponse r = pure $ r + 5
-- let processResponse r = result { return r + 5 }

writeResult :: Result -> Effect Unit
writeResult r = logShow r

run :: Effect (Either ErrorMessage Unit)
run = do
  resp <- ajaxRequest
  case resp of 
    Left err -> pure $ Left err
    Right r ->
      case (processResponse r) of 
        Left err -> pure $ Left err
        Right result -> writeResult result <#> Right

-- let run = io {
--   let! resp = ajaxRequest
--   match resp with
--   | Error e -> return Error e
--   | Ok r -> 
--     match processResponse r with
--     | Error e -> return Error e
--     | Ok result -> return! writeResult result |> IO.map Ok
-- }

main :: Effect Unit
main = do
  result <- run
  case result of 
    Left e -> log $ i "Process failed. "e
    Right _ -> log $ "Success!"

-- let main = io {
--   let! result = run
--   match result with
--   | Error e -> printfn "Process failed. %s" e
--   | Ok _ -> printfn "Success!"
-- }
