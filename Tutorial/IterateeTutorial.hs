sum1 :: [Int] -> Int
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

getNumber :: IO (Maybe Int)
getNumber = do
  x <- getLine
  if x == "q"
  then return Nothing
  else return $ Just $ read x

sum2 :: IO Int
sum2 = do
  maybeNum <- getNumber
  case maybeNum of
    Nothing -> return 0
    Just num -> do
         rest <- sum2
         return $ num + rest


data Stream a = Chunks [a] | EOF

getNumber2 :: IO (Stream Int)
getNumber2 = do
  maybeNum <- getNumber
  case maybeNum of
    Nothing -> return EOF
    Just num -> return $ Chunks [num]

sum3 :: IO Int
sum3 = do
  stream <- getNumber2
  case stream of
    EOF -> return 0
    Chunks nums -> do
        let nums' = sum nums
        rest <- sum3
        return $ nums' + rest

sum4 :: IO (Stream Int) -> IO Int
sum4 getNum = do
  stream <- getNum
  case stream of
    EOF -> return 0
    Chunks nums -> do
         let nums' = sum nums
         rest <- sum4 getNum
         return $ nums' + rest

data Step a m b = Error String
                | Yield b (Stream a)
                | Continue (Stream a -> Iteratee a m b)

newtype Iteratee a m b = Iteratee (m (Step a m b))

sum5 :: Monad m => Step Int m Int
sum5 =
    Continue $ go 0
  where
    go :: Monad m => Int -> Stream Int -> Iteratee Int m Int
    go runningSum (Chunks nums) = do
      let runningSum' = runningSum + sum nums
      Iteratee $ return $ Continue $ go runningSum'
    go runningSum EOF = Iteratee $ return $ Yield runningSum EOF

returnI :: Monad m => Step a m b -> Iteratee a m b
returnI = Iteratee . return

yield :: Monad m => b -> Stream a -> Iteratee a m b
yield x chunk = returnI $ Yield x chunk

continue :: Monad m => (Stream a -> Iteratee a m b) -> Iteratee a m b
continue f = returnI $ Continue f

sum5X :: Monad m => Step Int m Int
sum5X =
    Continue $ go 0
  where
    go :: Monad m => Int -> Stream Int -> Iteratee Int m Int
    go runningSum (Chunks nums) = do
      let runningSum' = runningSum + sum nums
      continue $ go runningSum'
    go runningSum EOF = yield runningSum EOF
