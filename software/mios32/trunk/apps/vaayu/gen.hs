#!/usr/bin/env runhaskell
import System.Environment

seconds_per_nop :: Double
seconds_per_nop = 10.39/(168e7)

main = do
    (us:_) <- getArgs
    putStrLn $ "asm volatile(\"" ++ (concat $ replicate (numNops us) "nop\\n\\t") ++ "\");"
        where
            numNops us = floor $ ((((read us) :: Double)/seconds_per_nop) / 1e6) * (10/14) * (10/12)
    
