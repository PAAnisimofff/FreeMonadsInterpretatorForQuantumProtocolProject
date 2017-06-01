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
data QMemory = QMemConstr [Int]
data CMemory = CMemConstr [Int]
data QPocket = QPoc [QBit]
data CPocket = CPoc [CBit]

data Memory = MemConstr QMemory CMemory QPocket CPocket

emptyMemory = MemConstr (QMemConstr []) (CMemConstr []) (QPoc []) (CPoc [])

emptyMemorySet = (emptyMemory, True, True, return ()) :: CoMemorySet

-- набор памяти
type CoMemorySet = (Memory, Bool, Bool, IO ())

-- общие функции

getQMemoryList :: Memory -> [Int]
getQMemoryList (MemConstr (QMemConstr qmemory) _ _ _) = qmemory

getCMemoryList :: Memory -> [Int]
getCMemoryList (MemConstr _ (CMemConstr cmemory) _ _) = cmemory

setQMemoryList :: Memory -> [Int] -> Memory
setQMemoryList (MemConstr _ cm qp cp) list = MemConstr (QMemConstr list) cm qp cp

setCMemoryList :: Memory -> [Int] -> Memory
setCMemoryList (MemConstr qm _ qp cp) list = MemConstr qm (CMemConstr list) qp cp

addQBits :: Memory -> Int -> ([Int], Memory)
addQBits (MemConstr (QMemConstr qm) cm qp cp) n = let last = endOrZero qm in
    let news = [last + 1 .. last + n] in
        (news, MemConstr (QMemConstr (qm ++ news)) cm qp cp)

addCBits :: Memory -> Int -> ([Int], Memory)
addCBits (MemConstr qm (CMemConstr cm) qp cp) n = let last = endOrZero cm in
    let news = [last + 1 .. last + n] in
        (news, MemConstr qm (CMemConstr (cm ++ news)) qp cp)

endOrZero :: [Int] -> Int
endOrZero [] = 0
endOrZero list  = last list 

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
tab :: IO ()
tab = putStr "\t"
-- актор

introduce :: Bool -> Bool -> IO ()
introduce nameFlag changeFlag = if changeFlag then introduceActor nameFlag else return ()

introduceActor :: Bool -> IO ()
introduceActor nameFlag = putStrLn $ "This is " ++ (actorName nameFlag) ++ "!"

actorsOut :: Bool -> IO ()
actorsOut nameFlag = putStrLn $ (actorName nameFlag) ++ "'s all!"

-- операции
qInitMessage :: [QBit] -> IO ()
qInitMessage qbitNs = putStrLn $ "- Init new qubits with numbers " ++ showQBits qbitNs ++ "."

cInitMessage :: [CBit] -> IO ()
cInitMessage cbitNs = putStrLn $ "- Init new classic bits with numbers " ++ showCBits cbitNs ++ "."

measureMessage :: [QBit] -> [CBit] -> IO ()
measureMessage qbits cbits = putStrLn $ "- Qubits with numbers " ++ showQBits qbits ++ " measured to classic bits with numbers" ++ showCBits cbits ++ "."

sendQMessageMessage :: [QBit] -> IO ()
sendQMessageMessage qbits = putStrLn $ "- Qubits with numbers " ++ showQBits qbits ++ " sent to partner."

recieveQMessageMessage :: [QBit] -> IO ()
recieveQMessageMessage qbits = putStrLn $ "- Qubits with numbers " ++ showQBits qbits ++ " recieved from partner."

sendCMessageMessage :: [CBit] -> IO ()
sendCMessageMessage cbits = putStrLn $ "- Classic bits with numbers " ++ showCBits cbits ++ " sent to partner."

recieveCMessageMessage :: [CBit] -> IO ()
recieveCMessageMessage cbits = putStrLn $ "- Classic bits with numbers " ++ showCBits cbits ++ " recieved from partner."

qAbstractGateMessage :: [QBit] -> IO ()
qAbstractGateMessage qbits = putStrLn $ "- Qubits with numbers " ++ showQBits qbits ++ " went through the Abstract Gate!"

qGateMessage :: [QBit] -> QGateDeterminant -> IO ()
qGateMessage qbits m = putStrLn $ "- Qubits with numbers " ++ showQBits qbits ++ " went through the gate with matrix:\n\n\t" ++ (replace "\n" "\n\t" (show m))

cGateMessage :: [CBit] -> СGateDeterminant -> IO ()
cGateMessage cbits m = putStrLn $ "- Classic bits with numbers " ++ showCBits cbits ++ " went through the gate."


-- реализация обработчиков комонадического интерпретатора

coQInit :: CoMemorySet -> [Bool] -> ([QBit], CoMemorySet)
coQInit (memory, nameFlag, changeFlag, io) bits = (qbits, (newMemory, nameFlag, False, newio)) 
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> qInitMessage qbits
        (qbitNs, newMemory) = addQBits memory (length bits)
        qbits = map QBit qbitNs

coCInit :: CoMemorySet -> [Bool] -> ([CBit], CoMemorySet)
coCInit (memory, nameFlag, changeFlag, io) bits = (cbits, (newMemory, nameFlag, False, newio)) 
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> cInitMessage cbits
        (cbitNs, newMemory) = addCBits memory (length bits)
        cbits = map CBit cbitNs

coMeasure :: CoMemorySet -> [QBit] -> ([CBit], CoMemorySet)
coMeasure (memory, nameFlag, changeFlag, io) qbits = (cbits, (newMemory, nameFlag, False, newio))
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> measureMessage qbits cbits
        (cbitNs, newMemory) = addCBits memory (length qbits)
        cbits = map CBit cbitNs

coQGate :: CoMemorySet -> [QBit] -> QGateDeterminant -> ([QBit], CoMemorySet)
coQGate (memory, nameFlag, changeFlag, io) qbits m = (qbits, (memory, nameFlag, False, newio))
    where 
        newio = io >> introduce nameFlag changeFlag >> tab >> qGateMessage qbits m

coCGate :: CoMemorySet -> [CBit] -> СGateDeterminant -> ([CBit], CoMemorySet)
coCGate (memory, nameFlag, changeFlag, io) cbits m = (cbits, (memory, nameFlag, False, newio))
    where 
        newio = io >> introduce nameFlag changeFlag >> tab >> cGateMessage cbits m

coSendQMessage :: CoMemorySet -> [QBit] -> CoMemorySet
coSendQMessage (memory, nameFlag, changeFlag, io) qbits = (newMemory, not nameFlag, True,  newio)
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> sendQMessageMessage qbits
        newMemory = putInQPocket memory qbits

coRecieveQMessage :: CoMemorySet -> ([QBit], CoMemorySet)
coRecieveQMessage (memory, nameFlag, changeFlag, io) = (qbits, (memory, nameFlag, False, newio))
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> recieveQMessageMessage qbits
        qbits = popFromQPocket memory

coSendCMessage :: CoMemorySet -> [CBit] -> CoMemorySet
coSendCMessage (memory, nameFlag, changeFlag, io) cbits = (newMemory, not nameFlag, True,  newio)
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> sendCMessageMessage cbits
        newMemory = putInCPocket memory cbits

coRecieveCMessage :: CoMemorySet -> ([CBit], CoMemorySet)
coRecieveCMessage (memory, nameFlag, changeFlag, io) = (cbits, (memory, nameFlag, False, newio))
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> recieveCMessageMessage cbits
        cbits = popFromCPocket memory