module LoggerTest where

import Lang.Lang
import Lang.Gates
import Logger.AdHoc
import Logger.Comonad

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
adhoc = simplyLog alice bob

-- комонадический интерпретатор
comonad = simplyLogCo alice bob