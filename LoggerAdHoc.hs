module LoggerAdHoc where

import System.IO
import Control.Monad.Free
import Lang
import LoggerCommon

-- ad-hoc интерпретатор
commandlog :: Memory -> Program () -> Program () -> Bool -> Bool -> IO ()
commandlog memory progM progS nameFlag changeFlag = 
    case progM of
    Free (QInit bits next) -> do
        introduce nameFlag changeFlag
        tab
        qInitMessage qbits
        commandlog newMemory (next qbits) progS nameFlag False 
        where
            (qbitNs, newMemory) = addQBits memory (length bits)
            qbits = map QBit qbitNs
    Free (CInit bits next) -> do
        introduce nameFlag changeFlag
        tab
        cInitMessage cbits
        commandlog newMemory (next cbits) progS nameFlag False 
        where
            (cbitNs, newMemory) = addCBits memory (length bits)
            cbits = map CBit cbitNs
    Free (Measure qbits next) -> do
        introduce nameFlag changeFlag
        tab
        measureMessage qbits cbits
        commandlog newMemory (next cbits) progS nameFlag False where
            (cbitNs, newMemory) = addCBits memory (length qbits)
            cbits = map CBit cbitNs
    Free (QGate qbits m next) -> do
        introduce nameFlag changeFlag
        tab
        qGateMessage qbits m
        commandlog memory (next qbits) progS nameFlag False
    Free (SendQMessage qbits next) -> do
        introduce nameFlag changeFlag
        tab
        sendQMessageMessage qbits
        commandlog newMemory progS (next) (not nameFlag) True where
            newMemory = putInQPocket memory qbits
    Free (RecieveQMessage next) -> do
        introduce nameFlag changeFlag
        tab
        recieveQMessageMessage qbits
        commandlog memory (next qbits) progS nameFlag False where
            qbits = popFromQPocket memory
    Free (SendCMessage cbits next) -> do
        introduce nameFlag changeFlag
        tab
        sendCMessageMessage cbits
        commandlog newMemory progS (next) (not nameFlag) True where
            newMemory = putInCPocket memory cbits
    Free (RecieveCMessage next) -> do
        introduce nameFlag changeFlag
        tab
        recieveCMessageMessage cbits
        commandlog memory (next cbits) progS nameFlag False where
            cbits = popFromCPocket memory
    Pure r -> do
        actorsOut nameFlag
        return r


-- вызов ad-hoc интепретатора
simplyLog :: Program () -> Program () -> IO ()
simplyLog alice bob = commandlog emptyMemory alice bob True True