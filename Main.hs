module Main where

import Lang
import LoggerAdHoc
import LoggerComonad

-- скрипт Алисы
alice :: Program ()
alice = do
    [a, b] <- qInit [True, True]
    c <- pauliYSingle a
    sendQMessage [b, c]
    e <- cInitSingle False
    sendCMessageSingle e
    return ()

-- скрипт Боба
bob :: Program ()
bob = do
    [a, b] <- recieveQMessage
    c <- pauliX [a, b]
    sendQMessage c
    e <- recieveCMessageSingle
    return ()

-- ad-hoc интерпретатор
main = simplyLog alice bob

-- комонадический интерпретатор
mainCo = simplyLogCo alice bob