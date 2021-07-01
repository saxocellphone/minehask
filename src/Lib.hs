module Lib
    ( someFunc
    ) where

data Foo = Bar

someFunc :: IO ()
someFunc = putStrLn "someFunc"

fooFunc :: a -> Foo
fooFunc a = Bar
