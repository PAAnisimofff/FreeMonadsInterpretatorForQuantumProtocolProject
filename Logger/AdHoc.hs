module Logger.AdHoc where

import System.IO
import Control.Monad.Free
import Lang.Lang
import Logger.Common

-- ad-hoc интерпретатор
commandlog :: CoMemorySet -> Program () -> Program () -> CoMemorySet
commandlog memory progM progS = 
    case progM of
    Free (QInit bits next) -> commandlog newMemory (next qbits) progS 
        where
            (qbits, newMemory) = coQInit memory bits
    Free (CInit bits next) -> commandlog newMemory (next cbits) progS 
        where
            (cbits, newMemory) = coCInit memory bits
    Free (Measure qbits next) -> commandlog newMemory (next cbits) progS
        where
            (cbits, newMemory) = coMeasure memory qbits
    Free (QGate qbits m next) -> commandlog newMemory (next newQbits) progS
        where
            (newQbits, newMemory) = coQGate memory qbits m
    Free (CGate cbits f next) -> commandlog newMemory (next newCbits) progS
        where
            (newCbits, newMemory) = coCGate memory cbits f
    Free (SendQMessage qbits next) -> commandlog newMemory progS (next) 
        where
            newMemory = coSendQMessage memory qbits
    Free (RecieveQMessage next) -> commandlog newMemory (next qbits) progS 
        where
            (qbits, newMemory) = coRecieveQMessage memory
    Free (SendCMessage cbits next) -> commandlog newMemory progS (next) 
        where
            newMemory = coSendCMessage memory cbits
    Free (RecieveCMessage next) -> commandlog newMemory (next cbits) progS 
        where
            (cbits, newMemory) = coRecieveCMessage memory
    Pure r -> memory


-- вызов ad-hoc интепретатора
simplyLog :: Program () -> Program () -> IO ()
simplyLog alice bob = io >> actorsOut nameFlag
    where
        (_,nameFlag,_, io) = commandlog emptyMemorySet alice bob