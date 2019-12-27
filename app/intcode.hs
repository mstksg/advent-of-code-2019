{-# LANGUAGE TypeApplications #-}

import           AOC.Common.Intcode
import           System.Environment

main :: IO ()
main = do
    x:_ <- getArgs
    Just mem <- parseMem <$> readFile x
    interactVM mem
