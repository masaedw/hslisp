import Control.Monad.Trans
import Data.Enumerator
import qualified Data.Enumerator.List as EL

sum6 :: Monad m => Iteratee Int m Int
sum6 = do
  maybeNum <- EL.head
  case maybeNum of
    Nothing -> return 0
    Just i -> do
         rest <- sum6
         return $ i + rest

sum6' :: Monad m => Iteratee Int m Int
sum6' = EL.fold (+) 0

lazyIO :: IO ()
lazyIO = do
  s <- lines `fmap` getContents
  mapM_ putStrLn s

interleaved :: MonadIO m => Iteratee String m ()
interleaved = do
  maybeLine <- EL.head
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      liftIO $ putStrLn line
      interleaved


