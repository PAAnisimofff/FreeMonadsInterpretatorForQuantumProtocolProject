module Logger where

import Data.String.Utils (replace)
import Lang.Lang
import Lang.Interpreters

-- память
data QMemory = QMemConstr [Int]
data CMemory = CMemConstr [Int]
data QPocket = QPoc [QBit]
data CPocket = CPoc [CBit]

data Memory = MemConstr QMemory CMemory QPocket CPocket

type MemorySet = (Memory, Bool, Bool, String)

emptyString = ""

emptyMemory = MemConstr (QMemConstr []) (CMemConstr []) (QPoc []) (CPoc [])

emptyMemorySet = (emptyMemory, True, True, emptyString)

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


-- реализация обработчиков интерпретатора

qInitLogger :: MemorySet -> [Bool] -> ([QBit], MemorySet)
qInitLogger (memory, nameFlag, changeFlag, message) bits = (qbits, (newMemory, nameFlag, False, newMessage)) 
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ qInitMessage qbits ++ "\n"
        (qbitNs, newMemory) = addQBits memory (length bits)
        qbits = map QBit qbitNs

cInitLogger :: MemorySet -> [Bool] -> ([CBit], MemorySet)
cInitLogger (memory, nameFlag, changeFlag, message) bits = (cbits, (newMemory, nameFlag, False, newMessage)) 
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ cInitMessage cbits ++ "\n"
        (cbitNs, newMemory) = addCBits memory (length bits)
        cbits = map CBit cbitNs

measureLogger :: MemorySet -> [QBit] -> ([CBit], MemorySet)
measureLogger (memory, nameFlag, changeFlag, message) qbits = (cbits, (newMemory, nameFlag, False, newMessage))
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ measureMessage qbits cbits ++ "\n"
        (cbitNs, newMemory) = addCBits memory (length qbits)
        cbits = map CBit cbitNs

qGateLogger :: MemorySet -> [QBit] -> QGateDeterminant -> ([QBit], MemorySet)
qGateLogger (memory, nameFlag, changeFlag, message) qbits m = (qbits, (memory, nameFlag, False, newMessage))
    where 
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ qGateMessage qbits ++ "\n"

cGateLogger :: MemorySet -> [CBit] -> СGateDeterminant -> ([CBit], MemorySet)
cGateLogger (memory, nameFlag, changeFlag, message) cbits m = (cbits, (memory, nameFlag, False, newMessage))
    where 
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ cGateMessage cbits ++ "\n"

sendQMessageLogger :: MemorySet -> [QBit] -> MemorySet
sendQMessageLogger (memory, nameFlag, changeFlag, message) qbits = (newMemory, not nameFlag, True,  newMessage)
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ sendQMessageMessage qbits ++ "\n"
        newMemory = putInQPocket memory qbits

recieveQMessageLogger :: MemorySet -> ([QBit], MemorySet)
recieveQMessageLogger (memory, nameFlag, changeFlag, message) = (qbits, (memory, nameFlag, False, newMessage))
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ recieveQMessageMessage qbits ++ "\n"
        qbits = popFromQPocket memory

sendCMessageLogger :: MemorySet -> [CBit] -> MemorySet
sendCMessageLogger (memory, nameFlag, changeFlag, message) cbits = (newMemory, not nameFlag, True,  newMessage)
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ sendCMessageMessage cbits ++ "\n"
        newMemory = putInCPocket memory cbits

recieveCMessageLogger :: MemorySet -> ([CBit], MemorySet)
recieveCMessageLogger (memory, nameFlag, changeFlag, message) = (cbits, (memory, nameFlag, False, newMessage))
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ recieveCMessageMessage cbits ++ "\n"
        cbits = popFromCPocket memory


logger :: MemorySet -> InterpreterF MemorySet
logger m = InterpreterF (qInitLogger m) (cInitLogger m) (measureLogger m) (qGateLogger m) (cGateLogger m) (sendQMessageLogger m) (recieveQMessageLogger m) (sendCMessageLogger m) (recieveCMessageLogger m)

-- вызов комонадного логгера
comonadLogger :: Program () -> Program () -> String
comonadLogger script1 script2 = message 
    where
        (_, _, _, message) = comonadInterpreterRun (logger) emptyMemorySet script1 script2

-- вызов ad-hoc интерпретатора
adhocLogger :: Program () -> Program () -> String
adhocLogger script1 script2 = message 
    where
        (_, _, _, message) = adhocInterpreterRun (logger) emptyMemorySet script1 script2