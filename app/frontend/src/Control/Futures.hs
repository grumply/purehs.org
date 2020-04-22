{-# language AllowAmbiguousTypes #-}
module Control.Futures 
  ( producing
  , producingKeyed
  , consuming
  , consumingWith
  , Options, defaultOptions, suspense, trouble
  ) where

import Control.Concurrent

import Pure.Elm hiding (Left,Right,Start,Options)

import Data.Foldable
import Data.Traversable
import Data.Typeable

data Model tag a = Model
  { producer :: Maybe ThreadId
  , evitable :: Maybe a
  }

data Message a
  = Start
  | Receive
  | Eventuated a
  | Shutdown

-- producing @Tag producer $ consuming consumer

producing 
  :: forall (tag :: *) a. (Typeable tag, Typeable a) 
  => IO a 
  -> (Maybe a -> View) 
  -> View
producing io = run (App [Start] [Receive] [Shutdown] mdl update view)
  where
    mdl = Model @tag Nothing Nothing
 
    update :: Elm (Message a) 
           => Message a 
           -> (Maybe a -> View) 
           -> Model tag a 
           -> IO (Model tag a)
    update Start _ Model {..} = do
      producer <- Just <$> forkIO (io >>= command . Eventuated)
      pure Model {..}

    update (Eventuated e) _ mdl =
      pure mdl { evitable = Just e }

    update Receive _ Model {..} = 
      pure Model {..}

    update Shutdown _ Model {..} =
      case producer of
        Just tid -> do
          killThread tid
          pure Model {..}
        _ -> 
          pure Model {..}

    view f Model {..} = f evitable

data KeyedModel tag key a = KeyedModel
  { key :: key
  , keyedProducer :: Maybe ThreadId 
  , keyedEvitable :: Maybe a
  }

producingKeyed
  :: forall (tag :: *) key a. (Eq key, Typeable tag, Typeable key, Typeable a) 
  => key
  -> (key -> IO a)
  -> (key -> Maybe a -> View) 
  -> View
producingKeyed key p f = run (App [Start] [Receive] [Shutdown] mdl update view) (key,p,f)
  where
    mdl = KeyedModel @tag key Nothing Nothing

    update :: Elm (Message a) 
           => Message a 
           -> (key,key -> IO a,key -> Maybe a -> View) 
           -> KeyedModel tag key a 
           -> IO (KeyedModel tag key a)
    update Start (k,p,_) KeyedModel {..} = do
      let io = p k
      keyedProducer <- Just <$> forkIO (io >>= command . Eventuated)
      pure KeyedModel {..}

    update (Eventuated (Just -> e)) _ mdl =
      pure mdl { keyedEvitable = e } 

    update Receive (k,p,_) KeyedModel {..}
      | k == key = pure KeyedModel {..}
      | otherwise = do
        for_ keyedProducer killThread
        let io = p k
        keyedProducer <- Just <$> forkIO (io >>= command . Eventuated)
        pure KeyedModel {..}

    update Shutdown _ KeyedModel {..} =
      case keyedProducer of
        Just tid -> do
          killThread tid
          let keyedProducer = Nothing
          pure KeyedModel {..}
        _ -> 
          pure KeyedModel {..}

    view (_,_,f) KeyedModel {..} =
      f key keyedEvitable

consuming :: (a -> View) -> Maybe a -> View
consuming = maybe Null

data ConsumerModel = ConsumerModel 
  { start           :: Time
  , suspenseMonitor :: Maybe ThreadId 
  , troubleMonitor  :: Maybe ThreadId
  , current         :: View
  }

data ConsumerMessage = ConsumerStart | ConsumerReceive | ConsumerCheckSuspense | ConsumerCheckTrouble

consumingWith 
  :: forall a. (Typeable a) 
  => Options 
  -> (a -> View) 
  -> Maybe a 
  -> View
consumingWith = \os f ma -> run (App [ConsumerStart] [ConsumerReceive] [] mdl update view) (os,ma,f)
  where
    mdl = ConsumerModel 0 Nothing Nothing Null

    startSuspenseMonitor start os = do
      elapsed <- subtract start <$> time
      let 
        delay' t
          | t > 0 = delay t
          | otherwise = pure ()
      for (_suspense os) $ \(d,_) -> forkIO $ do
        delay' (d - elapsed)
        command ConsumerCheckSuspense

    startTroubleMonitor start os = do
      elapsed <- subtract start <$> time
      let 
        delay' t
          | t > 0 = delay t
          | otherwise = pure ()
      for (_trouble os) $ \(d,_) -> forkIO $ do
        delay' (d - elapsed)
        command ConsumerCheckTrouble

    update ConsumerStart (os,_,_) ConsumerModel {..} = do
      start <- time
      suspenseMonitor <- startSuspenseMonitor start os
      troubleMonitor  <- startTroubleMonitor start os
      pure ConsumerModel {..}

    update ConsumerReceive (os,ma,f) ConsumerModel {..}
      | Just a <- ma = do
        for_ troubleMonitor  killThread
        for_ suspenseMonitor killThread
        let 
          current = f a
          suspenseMonitor = Nothing
          troubleMonitor = Nothing
        pure ConsumerModel {..}
      | otherwise = do
        for_ troubleMonitor  killThread
        for_ suspenseMonitor killThread
        suspenseMonitor <- startSuspenseMonitor start os
        troubleMonitor  <- startTroubleMonitor start os
        pure ConsumerModel {..}


    update ConsumerCheckSuspense (os,ma,_) ConsumerModel {..}
      | Just a <- ma = pure ConsumerModel {..}
      | otherwise = do
        now <- time

        -- make sure this ConsumerCheck is temporally sound
        case _suspense os of
          Just (delta,v)

            -- install suspense view
            | delta <= now - start ->
              let 
                current = v
                suspenseMonitor = Nothing
              in 
                pure ConsumerModel {..}

            -- re-fork the monitor
            | otherwise -> do
              suspenseMonitor <- startSuspenseMonitor start os
              pure ConsumerModel {..}

          _ -> 
            let suspenseMonitor = Nothing
            in pure ConsumerModel {..}

    update ConsumerCheckTrouble (os,ma,_) ConsumerModel {..}
      | Just a <- ma = pure ConsumerModel {..}
      | otherwise = do
        now <- time

        -- make sure this ConsumerCheck is temporally sound
        case _trouble os of
          Just (delta,v)

            -- install trouble view
            | delta <= now - start ->
              let 
                current = v
                troubleMonitor = Nothing
              in 
                pure ConsumerModel {..}

            -- re-fork the monitor
            | otherwise -> do
              troubleMonitor <- startTroubleMonitor start os
              pure ConsumerModel {..}

          _ ->
            let troubleMonitor = Nothing
            in pure ConsumerModel {..}

    view _ = current

data Options = Options
  { _suspense :: Maybe (Time,View)
  , _trouble  :: Maybe (Time,View)
  }

defaultOptions :: Options
defaultOptions = Options Nothing Nothing

suspense :: Time -> View -> Options -> Options
suspense t v os = os { _suspense = Just (t,v) }

trouble :: Time -> View -> Options -> Options
trouble t v os = os { _trouble = Just (t,v) }