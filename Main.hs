module Main where

import Lang
import SimplyLogger

alice :: Program ()
alice = do
    a <- qInit True
    b <- qGate a
    sendQMessage b
    c <- recieveQMessage
    d <- qGate c
    sendQMessage b
    return ()

bob :: Program ()
bob = do
    a <- recieveQMessage
    b <- qInit False
    sendQMessage b
    c <- recieveQMessage
    return ()

main = simplyLog alice bob