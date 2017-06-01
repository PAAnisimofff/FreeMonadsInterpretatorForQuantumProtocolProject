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
    e <- cGateSingle e (not)
    sendCMessageSingle e
    return ()

-- скрипт Боба
bob :: Program ()
bob = do
    a <- recieveQMessage
    c <- pauliX a
    sendQMessage c
    e <- recieveCMessageSingle
    d <- cInitSingle False
    b <- cGate [e, d] ((\x -> [x]) . or)
    return ()

-- ad-hoc интерпретатор
adhoc = simplyLog alice bob

-- комонадический интерпретатор
comonad = simplyLogCo alice bob