{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Sure(sure) where

class Sure m where sure :: m a -> a
instance Sure Maybe where sure (Just x) = x
instance Sure (Either e) where sure (Right x) = x
