module Main where

import Prelude

import Control.Monad.Except (ExceptT(..), except, lift, runExceptT)
import Control.Monad.State (StateT)
import Data.Either (Either(..))
import Data.Interpolate (i)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Class.Console (log, logShow)

type Result = Int
type Response = Int

type ErrorMessage = String

-- newtype EitherT a m b = EitherT (m (Either a b))
-- derive instance newtypeEitherT :: Newtype (EitherT a m b) _
-- instance monadEitherT :: Monad m => Monad (EitherT a m) 

-- instance applicativeEitherT :: Monad m => Applicative (EitherT err m) where 
--   -- let pure (x:'a) : IOResult<'a, 'err>) = 
--   --   IOResult <| io { return result { return x } } 
--   pure :: forall a. a -> EitherT err m a
--   pure x = EitherT $ pure $ pure x 

-- instance bindEitherT :: Monad m => Bind (EitherT err m) where
--   bind :: forall a b. EitherT err m a -> (a -> EitherT err m b) -> EitherT err m b
--   bind (EitherT (x :: m (Either err _))) fn = EitherT $ do
--     (x' :: Either err _) <- x
--     case x' of
--       Left e -> pure $ Left e
--       Right x'' -> 
--         let (EitherT (y :: m (Either err _))) = fn x''
--         in y

--   -- let bind (EitherT (x : IO<Result<'a, 'err>>)) (fn : 'a -> IOResult<'b, 'err>) : IOResult<'b, 'err> = IOResult <| io {
--   --   let! (x' : Result<'a, 'err>) = x
--   --   match x' with
--   --   | Error e -> return Error e
--   --   | Ok x'' ->
--   --     let (EitherT (y : IO<Result<'b, 'err>>)) = fn x''
--   --     return! y
--   -- }

-- instance applyEitherT :: Monad m => Apply (EitherT a m) where apply = ap
-- derive instance functorEitherT :: Monad m => Functor (EitherT a m) 

-- except :: forall err a m. Monad m => Either err a -> EitherT err m a
-- except x = EitherT $ pure x
-- -- let eitherToIOResult x = IOResult <| io { return x }

-- lift :: forall err a m. Monad m => m a -> EitherT err m a
-- lift x = EitherT $ do 
--   x' <- x
--   pure $ Right x'
-- -- let lift (x:IO<'a>) : IOResult<'a, 'err> = IOResult <| io {
-- --   let! (x':'a) = x
-- --   return Ok x'
-- -- }

type EffectStateEither err state a = ExceptT err (StateT state Effect) a



-- runEitherT :: forall err m a. Monad m => EitherT err m a -> m (Either err a)
-- runEitherT = unwrap
-- let runIOResult (IOResult x) = x

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
run = runExceptT $ do
  resp <- ExceptT ajaxRequest
  result <- except (processResponse resp)
  lift $ writeResult result 

-- f# |>, <|, |>>, <<|, =<<, >>= (loadWx dateRange =<< getConnStr) 
-- ps  #,  $, <#>, <$>, =<<, >>=
-- hs  &,  $, <&>, <$>, =<<, >>=


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
