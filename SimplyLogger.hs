{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module SimplyLogger where

import System.IO
import Control.Monad.Free
import Control.Comonad.Cofree
import Lang

-- память
data QMemory = QMemConstr [Int]
data CMemory = CMemConstr [Int]
data QPocket = QPoc [QBit]
data CPocket = CPoc [CBit]

data Memory = MemConstr QMemory CMemory QPocket CPocket

emptyMemory = MemConstr (QMemConstr []) (CMemConstr []) (QPoc []) (CPoc [])

type CoMemorySet = (Memory, Bool, IO ())


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

-- ad-hoc интерпретатор
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

-- обьединяет два скрипта в одну с переключаясь при отправке
mix :: Program r -> Program r -> Program r
mix progS progM = 
    case progM of
        Free (SendQMessage qbits next) -> Free (SendQMessage qbits ((mix next) progS))
        Free (SendCMessage cbits next) -> Free (SendCMessage cbits ((mix next) progS))
        Free a -> Free (fmap (mix progS) a)
        x -> x

-- пэйринг
class (Functor f, Functor g) => Pairing f g where
    pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing ((->) a) ((,) a) where
    pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
    pair p f g = pair (flip p) g f

instance Pairing f g => Pairing (Cofree f) (Free g) where
    pair p (a :< _ ) (Pure x)  = p a x
    pair p (_ :< fs) (Free gs) = pair (pair p) fs gs

-- комонадические функторы
data InterpreterF rez = InterpreterF {
    qInitHandle           :: ([Bool]           -> ([QBit], rez)),
    cInitHandle           :: ([Bool]           -> ([CBit], rez)),
    measureHandle         :: ([QBit]           -> ([CBit], rez)),
    qGateHandle           :: ([QBit]           -> ([QBit], rez)),
    sendQMessageHandle    ::  [QBit]           ->          rez  ,
    recieveQMessageHandle ::                      ([QBit], rez) ,
    sendCMessageHandle    ::  [CBit]           ->          rez  ,
    recieveCMessageHandle ::                      ([CBit], rez) ,
    hadamardHandle        :: ([QBit]           -> ([QBit], rez)),
    cNotHandle            :: (([QBit], [QBit]) -> ([QBit], rez)),
    pauliXHandle          :: ([QBit]           -> ([QBit], rez)),
    pauliYHandle          :: ([QBit]           -> ([QBit], rez)),
    pauliZHandle          :: ([QBit]           -> ([QBit], rez))
} deriving Functor

instance Pairing InterpreterF Command where
    pair f (InterpreterF qi _ _ _ _ _ _ _ _ _ _ _ _) (QInit           x   k) = pair f (qi x)   k
    pair f (InterpreterF _ ci _ _ _ _ _ _ _ _ _ _ _) (CInit           x   k) = pair f (ci x)   k
    pair f (InterpreterF _ _ m  _ _ _ _ _ _ _ _ _ _) (Measure         x   k) = pair f (m  x)   k
    pair f (InterpreterF _ _ _ qg _ _ _ _ _ _ _ _ _) (QGate           x   k) = pair f (qg x)   k
    pair f (InterpreterF _ _ _ _ sq _ _ _ _ _ _ _ _) (SendQMessage    x   k) =      f (sq x)   k
    pair f (InterpreterF _ _ _ _ _ rq _ _ _ _ _ _ _) (RecieveQMessage     k) = pair f  rq      k
    pair f (InterpreterF _ _ _ _ _ _ sc _ _ _ _ _ _) (SendCMessage    x   k) =      f (sc x)   k
    pair f (InterpreterF _ _ _ _ _ _ _ rc _ _ _ _ _) (RecieveCMessage     k) = pair f  rc      k
    pair f (InterpreterF _ _ _ _ _ _ _ _ h  _ _ _ _) (Hadamard        x   k) = pair f (h  x)   k
    pair f (InterpreterF _ _ _ _ _ _ _ _ _ cn _ _ _) (CNot            x y k) = pair f (cn (x, y)) k 
    pair f (InterpreterF _ _ _ _ _ _ _ _ _ _ px _ _) (PauliX          x   k) = pair f (px x)   k
    pair f (InterpreterF _ _ _ _ _ _ _ _ _ _ _ py _) (PauliY          x   k) = pair f (py x)   k
    pair f (InterpreterF _ _ _ _ _ _ _ _ _ _ _ _ pz) (PauliZ          x   k) = pair f (pz x)   k

type CoProgram a = Cofree InterpreterF a

-- собирает вместе монаду и комонаду
goTogether :: CoProgram CoMemorySet
goTogether = coiter next start
    where
        next w = InterpreterF (coQInit w) (coCInit w) (coMeasure w) (coQGate w) (coSendQMessage w) (coRecieveQMessage w) (coSendCMessage w) (coRecieveCMessage w) (coHadamard w) (coCNot w) (coPauliX w) (coPauliY w) (coPauliZ w) 
        start = (emptyMemory, True, return ())


-- реализация обработчиков комонадического интерпретатора

coQInit :: CoMemorySet -> [Bool] -> ([QBit], CoMemorySet)
coQInit (memory, nameFlag, io) bits = let name = actorName nameFlag in
    let (qbitNs, newMemory) = addQBits memory (length bits) in
        let newio = io >> putStrLn ("Init new qubits with numbers " ++ show qbitNs ++ ".\n") in
            (map QBitConstr qbitNs, (newMemory, nameFlag, newio))

coCInit :: CoMemorySet -> [Bool] -> ([CBit], CoMemorySet)
coCInit (memory, nameFlag, io) bits = let name = actorName nameFlag in
    let (cbitNs, newMemory) = addCBits memory (length bits) in
        let newio = io >> putStrLn ("Init new classic bits with numbers " ++ show cbitNs ++ ".\n") in
            (map CBitConstr cbitNs, (newMemory, nameFlag, newio))

coMeasure :: CoMemorySet -> [QBit] -> ([CBit], CoMemorySet)
coMeasure (memory, nameFlag, io) qbits =let name = actorName nameFlag in
    let (cbitNs, newMemory) = addCBits memory (length qbits) in
        let newio = io >> putStrLn ("This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " measured to classic bits with numbers" ++ show cbitNs ++ "\n") in
            (map CBitConstr cbitNs, (newMemory, nameFlag, newio))

coQGate :: CoMemorySet -> [QBit] -> ([QBit], CoMemorySet)
coQGate (memory, nameFlag, io) qbits = let name = actorName nameFlag in
    let newio = io >> putStrLn ("This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " went through the Abstract Unnamed Gate!\n") in
        (qbits, (memory, nameFlag, newio))

coSendQMessage :: CoMemorySet -> [QBit] -> CoMemorySet
coSendQMessage (memory, nameFlag, io) qbits = let name = actorName nameFlag in
    let newMemory = putInQPocket memory qbits in
        let  newio = io >> putStrLn( "This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " sent to partner.\n") in
            (newMemory, not nameFlag, newio)

coRecieveQMessage :: CoMemorySet -> ([QBit], CoMemorySet)
coRecieveQMessage (memory, nameFlag, io) = let name = actorName nameFlag in
    let qbits = popFromQPocket memory in
        let newio = io >> putStrLn ("This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " recieved from partner.\n") in
            (qbits, (memory, nameFlag, newio))

coSendCMessage :: CoMemorySet -> [CBit] -> CoMemorySet
coSendCMessage (memory, nameFlag, io) cbits = let name = actorName nameFlag in
    let newMemory = putInCPocket memory cbits in
        let newio = io >> putStrLn ("This is " ++ name ++ "\n" ++ "Classic bits with numbers " ++ showCBits cbits ++ " sent to partner.\n") in
            (newMemory, not nameFlag, newio)

coRecieveCMessage :: CoMemorySet -> ([CBit], CoMemorySet)
coRecieveCMessage (memory, nameFlag, io) = let name = actorName nameFlag in
    let cbits = popFromCPocket memory in
        let newio = io >> putStrLn ("This is " ++ name ++ "\n" ++ "Classic bits with numbers " ++ showCBits cbits ++ " recieved from partner.\n") in
            (cbits, (memory, nameFlag, newio))

coHadamard :: CoMemorySet -> [QBit] -> ([QBit], CoMemorySet)
coHadamard (memory, nameFlag, io) qbits = let name = actorName nameFlag in
    let newio = io >> putStrLn ("This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " went through the Hadamard Gate.\n") in
        (qbits, (memory, nameFlag, newio))

coCNot :: CoMemorySet -> ([QBit], [QBit]) -> ([QBit], CoMemorySet)
coCNot (memory, nameFlag, io) (qbitsC, qbits) = let name = actorName nameFlag in
    let newio = io >> putStrLn ("This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbitsC ++ " and " ++ showQBits qbits ++ " went through the CNot Gate.\n") in
        (qbitsC ++ qbits, (memory, nameFlag, newio))

coPauliX :: CoMemorySet -> [QBit] -> ([QBit], CoMemorySet)
coPauliX (memory, nameFlag, io) qbits =let name = actorName nameFlag in
    let newio = io >> putStrLn ("This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " went through the PauliX Gate.\n") in
        (qbits, (memory, nameFlag, newio))

coPauliY :: CoMemorySet -> [QBit] -> ([QBit], CoMemorySet)
coPauliY (memory, nameFlag, io) qbits =let name = actorName nameFlag in
    let newio = io >> putStrLn ("This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " went through the PauliY Gate.\n") in
        (qbits, (memory, nameFlag, newio))

coPauliZ :: CoMemorySet -> [QBit] -> ([QBit], CoMemorySet)
coPauliZ (memory, nameFlag, io) qbits =let name = actorName nameFlag in
    let newio = io >> putStrLn ("This is " ++ name ++ "\n" ++ "Qubits with numbers " ++ showQBits qbits ++ " went through the PauliZ Gate.\n") in
        (qbits, (memory, nameFlag, newio))


-- вызов ad-hoc интепретатора
simplyLog :: Program () -> Program () -> IO ()
simplyLog alice bob = commandlog emptyMemory alice bob True

-- вызов комонадического интерпретатора
simplyLogCo :: Program () -> Program () -> IO ()
simplyLogCo progM progS = io where
    (_, _, io) = pair const goTogether (mix progS progM)
