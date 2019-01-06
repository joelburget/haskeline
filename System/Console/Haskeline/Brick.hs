module System.Console.Haskeline.Brick ( configure
                                      , initialWidget
                                      , Widget
                                      , Config
                                      , ToBrick
                                      , handleEvent
                                      , useBrick
                                      , render
                                      ) where

import System.Console.Haskeline.Term
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads
import qualified System.Console.Haskeline.InputT as I
import qualified System.Console.Haskeline.Key as K

import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Catch

import Brick hiding (Widget, render)
import qualified Brick as B
import qualified Brick.BChan as BC
import qualified Graphics.Vty as V

import Control.Concurrent (MVar, putMVar, newEmptyMVar, takeMVar)
import Control.Concurrent.STM.TChan
import Control.Monad.STM

data Config e = MkConfig { fromBrickChan :: TChan Event
                         , toAppChan :: BC.BChan e
                         , toAppEventType :: ToBrick -> e
                         , fromAppEventType :: e -> Maybe ToBrick
                         }

data Widget n = MkWidget { name :: n
                         , visibleLines :: [String]
                         , hiddenLines :: [String]
                         , current :: (String, String)
                         , extent :: Maybe (Int, Int)
                         }

configure :: BC.BChan e
          -> (ToBrick -> e)
          -> (e -> Maybe ToBrick)
          -> IO (Config e)
configure toAppChan' toAppEventType' fromAppEventType' = do
    ch <- newTChanIO
    return $ MkConfig { fromBrickChan = ch
                      , toAppChan = toAppChan'
                      , toAppEventType = toAppEventType'
                      , fromAppEventType = fromAppEventType'
                      }

initialWidget :: n -> Widget n
initialWidget n = MkWidget { name = n
                           , visibleLines = []
                           , hiddenLines = []
                           , current = ("", "")
                           , extent = Nothing
                           }

data ToBrick = LayoutRequest (MVar (Maybe Layout))
             | MoveToNextLine
             | PrintLines [String]
             | DrawLineDiff LineChars
             | ClearLayout

handleEvent :: (Eq n) => Config e
            -> Widget n -> BrickEvent n e -> EventM n (Widget n)
handleEvent c w (AppEvent e) =
    case (fromAppEventType c) e of
      Just (LayoutRequest mv) -> do
          me <- lookupExtent (name w)
          case me of
            Just (Extent _ _ (wid, he) _) -> do
                liftIO . putMVar mv $ Just $ Layout wid he
                return $ w { extent = Just (wid, he) }
            Nothing -> do
                liftIO . putMVar mv $ Nothing
                return w

      Just MoveToNextLine -> do
          let (pre,suff) = current w
              w' = w { visibleLines = visibleLines w ++ [pre ++ suff]
                     , current = ("", "")
                     }
          let vp = viewportScroll (name w)
          vScrollToEnd vp
          return w'

      Just (PrintLines ls) -> do
          return $ w { visibleLines = visibleLines w ++ ls }

      Just (DrawLineDiff (pre, suff)) -> do
          return $ w { current = ( graphemesToString pre
                                 , graphemesToString suff) }

      Just ClearLayout -> do
          return $ w { visibleLines = []
                     , hiddenLines = hiddenLines w ++ visibleLines w
                     }

      Nothing -> return w

handleEvent c w (VtyEvent (V.EvKey k ms)) = do
    liftIO $ atomically $ writeTChan (fromBrickChan c) $ mkKeyEvent k
    return w
        where
            mkKeyEvent :: V.Key -> Event
            mkKeyEvent (V.KChar c') =
                KeyInput [ addModifiers ms $ K.simpleKey (K.KeyChar c') ]
            mkKeyEvent V.KEnter =
                KeyInput [ addModifiers ms $ K.simpleKey (K.KeyChar '\n') ]
            mkKeyEvent V.KBS =
                KeyInput [ addModifiers ms $ K.simpleKey K.Backspace ]
            mkKeyEvent V.KDel =
                KeyInput [ addModifiers ms $ K.simpleKey K.Delete ]
            mkKeyEvent V.KLeft =
                KeyInput [ addModifiers ms $ K.simpleKey K.LeftKey ]
            mkKeyEvent V.KRight =
                KeyInput [ addModifiers ms $ K.simpleKey K.RightKey ]
            mkKeyEvent V.KUp =
                KeyInput [ addModifiers ms $ K.simpleKey K.UpKey ]
            mkKeyEvent V.KDown =
                KeyInput [ addModifiers ms $ K.simpleKey K.DownKey ]
            mkKeyEvent _ = KeyInput []

            addModifiers :: [V.Modifier] -> K.Key -> K.Key
            addModifiers [] k' = k'
            addModifiers (V.MShift:tl) (K.Key m bc) =
                addModifiers tl $ (K.Key m { K.hasShift = True } bc)
            addModifiers (V.MCtrl:tl) (K.Key m (K.KeyChar c')) =
                addModifiers tl $ K.Key m (K.KeyChar $ K.setControlBits c')
            addModifiers (V.MCtrl:tl) k' = addModifiers tl . K.ctrlKey $ k'
            addModifiers (V.MMeta:tl) k' = addModifiers tl . K.metaKey $ k'
            addModifiers (V.MAlt:tl) k' = addModifiers tl k'

handleEvent _ w (VtyEvent (V.EvResize _ _)) = do
    me <- lookupExtent (name w)
    case me of
      Just (Extent _ _ (wid, he) _) -> do
          return $ w { extent = Just (wid, he) }
      Nothing -> return w

handleEvent _ w _ = return w

useBrick :: Config e -> I.Behavior
useBrick c = I.Behavior (brickRunTerm c)

brickRunTerm :: Config e -> IO RunTerm
brickRunTerm c = do
    let tops = TermOps { getLayout = getLayout'
                       , withGetEvent = withGetEvent'
                       -- saveUnusedKeys :: [Key] -> IO ()
                       -- saveKeys :: TChan Event -> [Key] -> IO ()
                       , saveUnusedKeys = saveKeys (fromBrickChan c)
                       , evalTerm = evalBrickTerm c
                       , externalPrint = atomically .
                           writeTChan (fromBrickChan c) . ExternalPrint
                       }
    return $ RunTerm { putStrOut = putStrOut'
                     , termOps = Left tops
                     , wrapInterrupt = id
                     , closeTerm = return ()
                     }
        where
            putStrOut' :: String -> IO ()
            putStrOut' s = do
                BC.writeBChan (toAppChan c) $
                    toAppEventType c $ PrintLines [s]

            getLayout' :: IO Layout
            getLayout' = do
                mv <- newEmptyMVar
                let e = toAppEventType c $ LayoutRequest mv
                BC.writeBChan (toAppChan c) e
                ml <- takeMVar mv
                case ml of
                  Just l -> return $ l
                  Nothing -> return $ Layout 0 0

            withGetEvent' :: forall m a . CommandMonad m
                          => (m Event -> m a) -> m a
            withGetEvent' f = f $ liftIO $ atomically $ readTChan $ fromBrickChan c

newtype BrickTerm m a =
    MkBrickTerm { unBrickTerm :: ReaderT (ToBrick -> IO ()) m a }
    deriving ( MonadIO, Monad, Applicative, Functor
             , MonadReader (ToBrick -> IO ())
             )

instance MonadTrans BrickTerm where
    lift = MkBrickTerm . lift

instance MonadThrow m => MonadThrow (BrickTerm m) where
  throwM = MkBrickTerm . throwM

instance MonadCatch m => MonadCatch (BrickTerm m) where
  catch (MkBrickTerm m) f = MkBrickTerm (catch m (unBrickTerm . f))

instance MonadMask m => MonadMask (BrickTerm m) where
  mask a = MkBrickTerm $ mask $ \u -> unBrickTerm (a $ q u)
    where q u = MkBrickTerm . u . unBrickTerm

  uninterruptibleMask a =
    MkBrickTerm $ uninterruptibleMask $ \u -> unBrickTerm (a $ q u)
      where q u = MkBrickTerm . u . unBrickTerm

  generalBracket acquire release use = MkBrickTerm $
    generalBracket
      (unBrickTerm acquire)
      (\resource exitCase -> unBrickTerm (release resource exitCase))
      (\resource -> unBrickTerm (use resource))

evalBrickTerm :: (CommandMonad m) => Config e -> EvalTerm m
evalBrickTerm c = EvalTerm
    (runReaderT' send . unBrickTerm)
    (MkBrickTerm . lift)
        where send = BC.writeBChan (toAppChan c) . toAppEventType c

instance (MonadReader Layout m, MonadMask m, MonadIO m)
  => Term (BrickTerm m) where
    reposition _ _ = return ()
    moveToNextLine _ = sendToBrick MoveToNextLine
    printLines ls = sendToBrick $ PrintLines ls
    drawLineDiff _ d = sendToBrick $ DrawLineDiff d
    clearLayout = sendToBrick $ ClearLayout
    ringBell _ = return ()

sendToBrick :: MonadIO m => ToBrick -> BrickTerm m ()
sendToBrick e = do
    f <- ask
    liftIO $ f e

render :: (Ord n, Show n) => Widget n -> B.Widget n
render (MkWidget { name = n
                 , current = (pre, suff)
                 , visibleLines = ls
                 , extent = mext
                 }) =
    reportExtent n $ viewport n Vertical $ prev <=> curr
        where
            prev = vBox $ map str $ concat $ map (wrap mext) ls
            curr = visible $ showCursor n (location mext) $
                vBox $ map str $ wrap mext $ pre ++ suff

            location Nothing = Location (length pre, 0)
            location (Just (w, _)) = let (q, r) = divMod (length pre) w in
                                     Location (r, q)

            wrap :: Maybe (Int, Int) -> String -> [String]
            wrap Nothing l = [l]
            wrap (Just (w, _)) l = go l
                where go xs = case splitAt w xs of
                                (ys, []) -> [ys]
                                (ys, tl) -> ys : go tl
