import Control.Monad.Free
import System.IO

data QBit = QBitConstr Int
data CBit = CBitConstr Int


data Command next = 
    QInit Bool (QBit -> next)    |
    CInit Bool (CBit -> next)    |
    Measure QBit (QBit -> next)  |
    QGate QBit (QBit -> next)

instance Functor Command where
    fmap f (QInit bit g) = QInit bit (f . g)
    fmap f (CInit bit g) = CInit bit (f . g)
    fmap f (Measure qbit g) = Measure qbit (f . g)
    fmap f (QGate  qbit g) = QGate qbit (f . g)

type Program = Free Command

qInit :: Bool -> Program QBit
qInit bit = liftF (QInit bit id)

cInit :: Bool -> Program CBit
cInit bit = liftF (CInit bit id)

measure :: QBit -> Program QBit
measure qbit = liftF (Measure qbit id)

qGate :: QBit -> Program QBit
qGate qbit = liftF (QGate qbit id)

prog :: Program ()
prog = do
    a <- qInit True
    b <- qGate a
    c <- measure b
    d <- cInit False
    return ()


data QMemory = QMemConstr [Int]
data CMemory = CMemConstr [Int]
data QPocket = QPoc Int | QEmpty
data CPocket = CPoc Int | CEmpty

data Memory = MemConstr QMemory CMemory QPocket CPocket

getQMemoryList :: Memory -> [Int]
getQMemoryList (MemConstr (QMemConstr qmemory) _ _ _) = qmemory

getCMemoryList :: Memory -> [Int]
getCMemoryList (MemConstr _ (CMemConstr cmemory) _ _) = cmemory

addIncQ :: Memory -> (Int, Memory)
addIncQ (MemConstr (QMemConstr qm) cm qp cp) = let new = (end qm) + 1 
    in let newQm =  QMemConstr (qm ++ [new])
    in (new, MemConstr newQm cm qp cp) where

addIncC :: Memory -> (Int, Memory)
addIncC (MemConstr qm (CMemConstr cm) qp cp) = let new = (end cm) + 1 
    in let newCm =  CMemConstr (cm ++ [new])
    in (new, MemConstr qm newCm qp cp) where

end :: [Int] -> Int
end [] = 0
end list  = last list 


goodcommandlog :: Memory -> Program () -> IO ()
goodcommandlog memory prog = case prog of
    Free (QInit bit next) -> do
        putStrLn "QInit"
        let (c, mem) = addIncQ memory in do
            putStrLn ("Init new qubit with number " ++ show c ++ "\n") 
            goodcommandlog mem (next (QBitConstr c))
    Free (CInit bit next) -> do
        putStrLn "CInit"
        let (c, mem) = addIncC memory in do
            putStrLn ("Init new classic bit with number " ++ show c ++ "\n") 
            goodcommandlog mem (next (CBitConstr c))
    Free (Measure (QBitConstr qbitN) next) -> do
        putStrLn ("Measure\n" ++ "Qubit with number " ++ show qbitN ++ " measured\n")
        goodcommandlog memory (next (QBitConstr qbitN))    
    Free (QGate (QBitConstr qbitN) next) -> do
        putStrLn ("Gate\n" ++ "Qubit with number " ++ show qbitN ++ " went throgh the Gate!\n")
        goodcommandlog memory (next (QBitConstr qbitN))
    Pure r -> do
        putStrLn "That's all!" 
        return r

emptyMemory = MemConstr (QMemConstr []) (CMemConstr []) QEmpty CEmpty

main = goodcommandlog emptyMemory prog