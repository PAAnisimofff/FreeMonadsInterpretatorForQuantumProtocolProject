{-# LANGUAGE DeriveFunctor #-}
module Logger.Common where

import System.IO
-- import Control.Monad.Free
-- import Control.Comonad.Cofree
import Data.String.Utils (replace)
import Lang.Lang
import Common.Matrix
import Common.Complex

-- комонадические функторы
data InterpreterF rez = InterpreterF {
    qInitHandle           :: ([Bool]                     -> ([QBit], rez)),
    cInitHandle           :: ([Bool]                     -> ([CBit], rez)),
    measureHandle         :: ([QBit]                     -> ([CBit], rez)),
    qGateHandle           :: ([QBit] -> QGateDeterminant -> ([QBit], rez)),
    cGateHandle           :: ([CBit] -> СGateDeterminant -> ([CBit], rez)),
    sendQMessageHandle    ::  [QBit]                     ->          rez  ,
    recieveQMessageHandle ::                                ([QBit], rez) ,
    sendCMessageHandle    ::  [CBit]                     ->          rez  ,
    recieveCMessageHandle ::                                ([CBit], rez) 
} deriving Functor

-- память
data QPocket = QPoc [QBit]
data CPocket = CPoc [CBit]

data Memory = MemConstr Int Int QPocket CPocket

type CoMemorySet = (Memory, Bool, Bool, String)

emptyString = ""

initialCounter = 0

emptyMemory = MemConstr initialCounter initialCounter (QPoc []) (CPoc [])

emptyMemorySet = (emptyMemory, True, True, emptyString)

-- общие функции

addQBits :: Memory -> Int -> ([Int], Memory)
addQBits (MemConstr qCounter cm qp cp) n = 
    ([qCounter + 1 .. qCounter + n], MemConstr (qCounter + n) cm qp cp)

addCBits :: Memory -> Int -> ([Int], Memory)
addCBits (MemConstr qm cCounter qp cp) n = 
    ([cCounter + 1 .. cCounter + n], MemConstr qm (cCounter + n) qp cp)

putInQPocket :: Memory -> [QBit] -> Memory
putInQPocket (MemConstr qm cm _ cp) x =  MemConstr qm cm (QPoc x) cp

popFromQPocket :: Memory -> [QBit]
popFromQPocket (MemConstr qm cm (QPoc x) cp) = x

putInCPocket :: Memory -> [CBit] -> Memory
putInCPocket (MemConstr qm cm qp _) x =  MemConstr qm cm qp (CPoc x)

popFromCPocket :: Memory -> [CBit]
popFromCPocket (MemConstr qm cm qp (CPoc x)) = x

actorName :: Bool -> String
actorName nameFlag = if nameFlag then "Alice" else "Bob"

showQBits :: [QBit] -> String
showQBits qbits = show $ map exractQNumber qbits

showCBits :: [CBit] -> String
showCBits cbits = show $ map exractCNumber cbits


-- сообщения

-- всяко
tab :: String
tab = "\t"
-- актор

introduce :: Bool -> Bool -> String
introduce nameFlag changeFlag = if changeFlag then introduceActor nameFlag else emptyString

introduceActor :: Bool -> String
introduceActor nameFlag = "This is " ++ (actorName nameFlag) ++ "!" ++ "\n"

actorsOut :: Bool -> String
actorsOut nameFlag = (actorName nameFlag) ++ "'s all!" ++ "\n"

-- операции
qInitMessage :: [QBit] -> String
qInitMessage qbitNs = "- Init new qubits with numbers " ++ showQBits qbitNs ++ "."

cInitMessage :: [CBit] -> String
cInitMessage cbitNs = "- Init new classic bits with numbers " ++ showCBits cbitNs ++ "."

measureMessage :: [QBit] -> [CBit] -> String
measureMessage qbits cbits = "- Qubits with numbers " ++ showQBits qbits ++ " measured to classic bits with numbers" ++ showCBits cbits ++ "."

sendQMessageMessage :: [QBit] -> String
sendQMessageMessage qbits = "- Qubits with numbers " ++ showQBits qbits ++ " sent to partner."

recieveQMessageMessage :: [QBit] -> String
recieveQMessageMessage qbits = "- Qubits with numbers " ++ showQBits qbits ++ " recieved from partner."

sendCMessageMessage :: [CBit] -> String
sendCMessageMessage cbits = "- Classic bits with numbers " ++ showCBits cbits ++ " sent to partner."

recieveCMessageMessage :: [CBit] -> String
recieveCMessageMessage cbits = "- Classic bits with numbers " ++ showCBits cbits ++ " recieved from partner."

qAbstractGateMessage :: [QBit] -> String
qAbstractGateMessage qbits = "- Qubits with numbers " ++ showQBits qbits ++ " went through the Abstract Gate!"

qGateMessage :: [QBit] -> String
qGateMessage qbits = "- Qubits with numbers " ++ showQBits qbits ++ " went through the some gate."

cGateMessage :: [CBit] -> String
cGateMessage cbits = "- Classic bits with numbers " ++ showCBits cbits ++ " went through some gate."


-- реализация обработчиков комонадического интерпретатора

coQInit :: CoMemorySet -> [Bool] -> ([QBit], CoMemorySet)
coQInit (memory, nameFlag, changeFlag, message) bits = (qbits, (newMemory, nameFlag, False, newMessage)) 
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ qInitMessage qbits ++ "\n"
        (qbitNs, newMemory) = addQBits memory (length bits)
        qbits = map QBit qbitNs

coCInit :: CoMemorySet -> [Bool] -> ([CBit], CoMemorySet)
coCInit (memory, nameFlag, changeFlag, message) bits = (cbits, (newMemory, nameFlag, False, newMessage)) 
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ cInitMessage cbits ++ "\n"
        (cbitNs, newMemory) = addCBits memory (length bits)
        cbits = map CBit cbitNs

coMeasure :: CoMemorySet -> [QBit] -> ([CBit], CoMemorySet)
coMeasure (memory, nameFlag, changeFlag, message) qbits = (cbits, (newMemory, nameFlag, False, newMessage))
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ measureMessage qbits cbits ++ "\n"
        (cbitNs, newMemory) = addCBits memory (length qbits)
        cbits = map CBit cbitNs

coQGate :: CoMemorySet -> [QBit] -> QGateDeterminant -> ([QBit], CoMemorySet)
coQGate (memory, nameFlag, changeFlag, message) qbits m = (qbits, (memory, nameFlag, False, newMessage))
    where 
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ qGateMessage qbits ++ "\n"

coCGate :: CoMemorySet -> [CBit] -> СGateDeterminant -> ([CBit], CoMemorySet)
coCGate (memory, nameFlag, changeFlag, message) cbits m = (cbits, (memory, nameFlag, False, newMessage))
    where 
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ cGateMessage cbits ++ "\n"

coSendQMessage :: CoMemorySet -> [QBit] -> CoMemorySet
coSendQMessage (memory, nameFlag, changeFlag, message) qbits = (newMemory, not nameFlag, True,  newMessage)
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ sendQMessageMessage qbits ++ "\n"
        newMemory = putInQPocket memory qbits

coRecieveQMessage :: CoMemorySet -> ([QBit], CoMemorySet)
coRecieveQMessage (memory, nameFlag, changeFlag, message) = (qbits, (memory, nameFlag, False, newMessage))
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ recieveQMessageMessage qbits ++ "\n"
        qbits = popFromQPocket memory

coSendCMessage :: CoMemorySet -> [CBit] -> CoMemorySet
coSendCMessage (memory, nameFlag, changeFlag, message) cbits = (newMemory, not nameFlag, True,  newMessage)
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ sendCMessageMessage cbits ++ "\n"
        newMemory = putInCPocket memory cbits

coRecieveCMessage :: CoMemorySet -> ([CBit], CoMemorySet)
coRecieveCMessage (memory, nameFlag, changeFlag, message) = (cbits, (memory, nameFlag, False, newMessage))
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ recieveCMessageMessage cbits ++ "\n"
        cbits = popFromCPocket memory