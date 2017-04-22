module Main where

import Lang
import SimplyLogger

-- чисто потестить

alice :: Program ()
alice = do
    [a, b] <- qInit [True, True]
    [c] <- qGate [a]
    sendQMessage [b, c]
    [e] <- cInit [False]
    sendCMessage [e]
    return ()

bob :: Program ()
bob = do
    [a, b] <- recieveQMessage
    c <- qGate [a, b]
    sendQMessage c
    e <- recieveCMessage
    return ()

main = simplyLog alice bob