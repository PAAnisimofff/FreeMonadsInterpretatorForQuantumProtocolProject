module Main where

import Lang
import SimplyLogger

-- скрипт Алисы
alice :: Program ()
alice = do
    [a, b] <- qInit [True, True]
    [c] <- qGate [a]
    sendQMessage [b, c]
    [e] <- cInit [False]
    sendCMessage [e]
    return ()

-- скрипт Боба
bob :: Program ()
bob = do
    [a, b] <- recieveQMessage
    c <- qGate [a, b]
    sendQMessage c
    e <- recieveCMessage
    return ()

-- ad-hoc интерпретатор
main = simplyLog alice bob

-- комонадический интерпретатор
mainCo = simplyLogCo alice bob