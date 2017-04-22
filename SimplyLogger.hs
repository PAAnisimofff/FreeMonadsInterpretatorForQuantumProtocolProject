module SimplyLogger where

import System.IO
import Control.Monad.Free
import Lang

data QMemory = QMemConstr [Int]
data CMemory = CMemConstr [Int]
data QPocket = QPoc [QBit]
data CPocket = CPoc [CBit]

data Memory = MemConstr QMemory CMemory QPocket CPocket

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

-- интерприторот
commandlog :: Memory -> Program () -> Program () -> Bool -> IO ()
commandlog memory progM progS nameFlag = 
    let name = actorName nameFlag in
        case progM of
        Free (QInit bitList next) -> do
            putStrLn $ "This is " ++ name
            let (qbitNs, newMemory) = addQBits memory (length bitList) in do
                putStrLn $ "Init new qubits with numbers " ++ show qbitNs ++ ".\n"
                commandlog newMemory (next (map QBitConstr qbitNs)) progS nameFlag
        Free (CInit bitList next) -> do
            putStrLn $ "This is " ++ name
            let (cbitNs, newMemory) = addCBits memory (length bitList) in do
                putStrLn $ "Init new classic bits with numbers " ++ show cbitNs ++ ".\n"
                commandlog newMemory (next (map CBitConstr cbitNs)) progS nameFlag
        Free (Measure qbits next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " measured to classic bits with numbers" ++ show cbitNs ++ "\n"
            commandlog newMemory (next (map CBitConstr cbitNs)) progS nameFlag where
                (cbitNs, newMemory) = addCBits memory (length qbits)
        Free (QGate qbits next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " went through the Abstract Unnamed Gate!\n"
            commandlog memory (next qbits) progS nameFlag
        Free (SendQMessage qbits next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " sent to partner.\n"
            commandlog newMemory progS (next) (not nameFlag) where
                newMemory = putInQPocket memory qbits
        Free (RecieveQMessage next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " recieved from partner.\n"
            commandlog memory (next qbits) progS nameFlag where
                qbits = popFromQPocket memory
        Free (SendCMessage cbits next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Classic bits with numbers " ++ showCBits cbits ++ " sent to partner.\n"
            commandlog newMemory progS (next) (not nameFlag) where
                newMemory = putInCPocket memory cbits
        Free (RecieveCMessage next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Classic bits with numbers " ++ showCBits cbits ++ " recieved from partner.\n"
            commandlog memory (next cbits) progS nameFlag where
                cbits = popFromCPocket memory
        Free (Hadamard qbits next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " went through the Hadamard Gate.\n"
            commandlog memory (next qbits) progS nameFlag
        Free (PauliX qbits next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " went through the PauliX Gate.\n"
            commandlog memory (next qbits) progS nameFlag
        Free (PauliY qbits next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " went through the PauliY Gate.\n"
            commandlog memory (next qbits) progS nameFlag
        Free (PauliZ qbits next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " went through the PauliZ Gate.\n"
            commandlog memory (next qbits) progS nameFlag
        Free (CNot qbitsC qbits next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbitsC ++ " and " ++ showQBits qbits ++ " went through the CNot Gate.\n"
            commandlog memory (next (qbitsC ++ qbits)) progS nameFlag
        Pure r -> do
            putStrLn $ name ++ "'s all!"
            return r

emptyMemory = MemConstr (QMemConstr []) (CMemConstr []) (QPoc []) (CPoc [])

simplyLog :: Program () -> Program () -> IO ()
simplyLog alice bob = commandlog emptyMemory alice bob True