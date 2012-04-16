module Data.History where

data History a = History { past    :: [a]
                         , present :: a
                         , future  :: [a]
                         }

historyNew :: a -> History a
historyNew x = History [] x []

historyAppend :: History a -> a -> History a
historyAppend (History past present future) x = History past present (future ++ [x])

historyPrepend :: History a -> a -> History a
historyPrepend (History past present future) x = History (past ++ [x]) present future

historyNext :: History a -> History a
historyNext h@(History _ _ []) = h
historyNext (History past present (f:future)) = History (present:past) f future

historyBack :: History a -> History a
historyBack h@(History [] _ _) = h
historyBack (History (p:past) present future) = History past p (present:future)

historyForgetPast :: History a -> History a
historyForgetPast (History _ present future) = History [] present future

historyForgetFuture :: History a -> History a
historyForgetFuture (History past present _) = History past present []

historyBranch :: History a -> a -> History a
historyBranch (History past present _) newPresent = History (present:past) newPresent []

historySize :: History a -> Int
historySize (History past _ future) = length past + length future + 1

historyPresent :: History a -> a
historyPresent (History _ x _) = x

historyDropPast :: History a -> Int -> History a
historyDropPast h@(History past present future) len = History (take len past) present future

historyTrim :: History a -> Int -> History a
historyTrim h@(History past present future) len
  | len <= 0  = h
  | len > fut = historyDropPast h (len - fut)
  | otherwise = historyTrim (historyNext h) len
 where fut = length future + 1
