module SimplyLogger where

import System.IO
import Control.Monad.Free
import Lang

data QMemory = QMemConstr [Int]
data CMemory = CMemConstr [Int]
data QPocket = QPoc QBit
data CPocket = CPoc CBit

data Memory = MemConstr QMemory CMemory QPocket CPocket

getQMemoryList :: Memory -> [Int]
getQMemoryList (MemConstr (QMemConstr qmemory) _ _ _) = qmemory

getCMemoryList :: Memory -> [Int]
getCMemoryList (MemConstr _ (CMemConstr cmemory) _ _) = cmemory

setQMemoryList :: Memory -> [Int] -> Memory
setQMemoryList (MemConstr _ cm qp cp) list = MemConstr (QMemConstr list) cm qp cp

setCMemoryList :: Memory -> [Int] -> Memory
setCMemoryList (MemConstr qm _ qp cp) list = MemConstr qm (CMemConstr list) qp cp

addIncQ :: Memory -> (Int, Memory)
addIncQ (MemConstr (QMemConstr qm) cm qp cp) = let new = (endOrZero qm) + 1 
    in let newQm =  QMemConstr (qm ++ [new])
    in (new, MemConstr newQm cm qp cp) where

addIncC :: Memory -> (Int, Memory)
addIncC (MemConstr qm (CMemConstr cm) qp cp) = let new = (endOrZero cm) + 1 
    in let newCm =  CMemConstr (cm ++ [new])
    in (new, MemConstr qm newCm qp cp) where

endOrZero :: [Int] -> Int
endOrZero [] = 0
endOrZero list  = last list 

putInQPocket :: Memory -> QBit -> Memory
putInQPocket (MemConstr qm cm _ cp) qBit =  MemConstr qm cm (QPoc qBit) cp

popFromQPocket :: Memory -> QBit
popFromQPocket (MemConstr qm cm (QPoc qBit) cp) = qBit

putInCPocket :: Memory -> CBit -> Memory
putInCPocket (MemConstr qm cm qp _) cBit =  MemConstr qm cm qp (CPoc cBit)

popFromCPocket :: Memory -> CBit
popFromCPocket (MemConstr qm cm qp (CPoc cBit)) = cBit

actorName :: Bool -> String
actorName nameFlag = if nameFlag then "Alice" else "Bob"

-- интерприторот
commandlog :: Memory -> Program () -> Program () -> Bool -> IO ()
commandlog memory progM progS nameFlag = 
    let name = actorName nameFlag in
        case progM of
        Free (QInit bit next) -> do
            putStrLn $ "This is " ++ name
            let (c, mem) = addIncQ memory in do
                putStrLn $ "Init new qubit with number " ++ show c ++ ".\n"
                commandlog mem (next (QBitConstr c)) progS nameFlag
        Free (CInit bit next) -> do
            putStrLn $ "This is " ++ name
            let (c, mem) = addIncC memory in do
                putStrLn $ "Init new classic bit with number" ++ show c ++ ".\n"
                commandlog mem (next (CBitConstr c)) progS nameFlag
        Free (Measure (QBitConstr qbitN) next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubit with number " ++ show qbitN ++ " measured.\n"
            commandlog memory (next (QBitConstr qbitN)) progS nameFlag
        Free (QGate (QBitConstr qbitN) next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubit with number " ++ show qbitN ++ " went through the Gate!\n"
            commandlog memory (next (QBitConstr qbitN)) progS nameFlag
        Free (SendQMessage (QBitConstr qbitN) next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubit with number " ++ show qbitN ++ " sent to partner.\n"
            commandlog mem progS (next) (not nameFlag) where
                mem = putInQPocket memory (QBitConstr qbitN) 
        Free (RecieveQMessage next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Qubit with number " ++ show qbitN ++ " recieved from partner.\n"
            commandlog memory (next (QBitConstr qbitN)) progS nameFlag where
                (QBitConstr qbitN) = popFromQPocket memory
        Free (SendCMessage (CBitConstr qbitN) next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Classic bit with number " ++ show qbitN ++ " sent to partner.\n"
            commandlog mem progS (next) (not nameFlag) where
                mem = putInCPocket memory (CBitConstr qbitN) 
        Free (RecieveCMessage next) -> do
            putStrLn $ "This is " ++ name ++ "\n" ++ "Classic bit with number " ++ show cbitN ++ " recieved from partner.\n"
            commandlog memory (next (CBitConstr cbitN)) progS nameFlag where
                (CBitConstr cbitN) = popFromCPocket memory
        Pure r -> do
            putStrLn $ name ++ "'s all!"
            return r

emptyMemory = MemConstr (QMemConstr []) (CMemConstr []) (QPoc (QBitConstr 0)) (CPoc (CBitConstr 0))

simplyLog :: Program () -> Program () -> IO ()
simplyLog alice bob = commandlog emptyMemory alice bob True