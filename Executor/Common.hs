{-# LANGUAGE DeriveFunctor #-}
module Logger.Common where

import System.IO
-- import Control.Monad.Free
-- import Control.Comonad.Cofree
import Data.String.Utils (replace)
import Lang.Lang
import Common.Matrix
import Common.Complex
import qualified Data.Vector as V

-- комонадические функторы
data InterpreterF rez = InterpreterF {
    qInitHandle           :: ([Bool]                     -> rez),
    cInitHandle           :: ([Bool]                     -> rez),
    measureHandle         :: ([QBit]                     -> rez),
    qGateHandle           :: ([QBit] -> QGateDeterminant -> rez),
    cGateHandle           :: ([CBit] -> СGateDeterminant -> rez),
    sendQMessageHandle    ::  [QBit]                     ->          rez  ,
    recieveQMessageHandle ::                                ([QBit], rez) ,
    sendCMessageHandle    ::  [CBit]                     ->          rez  ,
    recieveCMessageHandle ::                                ([CBit], rez) 
} deriving Functor

-- память
data QMemory = QMemConstr [(Complex Double, Complex Double)]
data CMemory = CMemConstr [Bool]
data QPocket = QPoc [QBit]
data CPocket = CPoc [CBit]

data Memory = MemConstr QMemory CMemory QPocket CPocket

type CoMemorySet = (Memory, Bool, Bool)

emptyString = ""

emptyMemory = MemConstr (QMemConstr []) (CMemConstr []) (QPoc []) (CPoc [])

emptyMemorySet = (emptyMemory, True, True)

-- общие функции

getQMemoryList :: Memory -> [(Complex Double, Complex Double)]
getQMemoryList (MemConstr (QMemConstr qmemory) _ _ _) = qmemory

getCMemoryList :: Memory -> [Bool]
getCMemoryList (MemConstr _ (CMemConstr cmemory) _ _) = cmemory

setQMemoryList :: Memory -> [(Complex Double, Complex Double)] -> Memory
setQMemoryList (MemConstr _ cm qp cp) list = MemConstr (QMemConstr list) cm qp cp

setCMemoryList :: Memory -> [Bool] -> Memory
setCMemoryList (MemConstr qm _ qp cp) list = MemConstr qm (CMemConstr list) qp cp

addQBits :: Memory -> [(Complex Double, Complex Double)] -> Memory
addQBits (MemConstr (QMemConstr qm) cm qp cp) income = MemConstr (QMemConstr (qm ++ income)) cm qp cp
        

addCBits :: Memory -> [Bool] -> Memory
addCBits (MemConstr qm (CMemConstr cm) qp cp) income = MemConstr qm (CMemConstr (cm ++ income)) qp cp

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

boolToCD :: Bool -> (Complex Double, Complex Double)
boolToCD True = (1.0:+0.0, 0.0:+0.0)
boolToCD False = (0.0:+0.0, 1.0:+0.0)

double_to_list :: (Complex Double, Complex Double) -> [Complex Double]
double_to_list (cd1, cd2) = [cd1, cd2]

boolsToCDs :: [Bool] -> [(Complex Double, Complex Double)]
boolsToCDs bs = map boolToCD bs

cdToBool :: [Complex Double] -> [Bool]
cdToBool [1.0:+0.0, 0.0:+0.0] = [True]
cdToBool [0.0:+0.0, 1.0:+0.0] = [False]


searchSafe :: [Int] -> [a] -> [a]
searchSafe indices xs = go indices 0 xs
   where
   go :: [Int] -> Int -> [a] -> [a]
   go []     _ _                = []
   go _      _ []               = error "index not found"
   go (i:is) j yys@(y:_) | i==j = y : go is  j     yys
   go iis    j (_:ys)           =     go iis (j+1) ys


searchUnsafe :: [Int] -> [a] -> [a]
searchUnsafe indexes list = [list!!x | x <- indexes]

cd = [1.0:+0.0, 0.0:+0.0, 0.0:+0.0, 1.0:+0.0]
-- сообщения

-- всяко
tab :: String
tab = "\t"


-- реализация обработчиков комонадического интерпретатора

coQInit :: CoMemorySet -> [Bool] -> CoMemorySet
coQInit (memory, nameFlag, changeFlag) bits = (newMemory, nameFlag, False)
    where
        newMemory = addQBits memory (boolsToCDs bits)

coCInit :: CoMemorySet -> [Bool] -> CoMemorySet
coCInit (memory, nameFlag, changeFlag) bits = (newMemory, nameFlag, False)
    where
        newMemory = addCBits memory bits

coCGate :: CoMemorySet -> [CBit] -> СGateDeterminant -> CoMemorySet
coCGate (memory, nameFlag, changeFlag) cbits m = (memory, nameFlag, False)
    


{-coMeasure :: CoMemorySet -> [QBit] -> CoMemorySet
coMeasure (memory, nameFlag, changeFlag) qbits = (newMemory, nameFlag, False)
    where
        (cbitNs, newMemory) = addCBits memory (length qbits)
        cbits = map CBit cbitNs
      --  newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ measureMessage qbits cbits ++ "\n"
      

coQGate :: CoMemorySet -> [QBit] -> QGateDeterminant -> CoMemorySet
coQGate (memory, nameFlag, changeFlag) qbits m = (memory, nameFlag, False)
    where 
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ qGateMessage qbits ++ "\n"

coCGate :: CoMemorySet -> [CBit] -> СGateDeterminant -> CoMemorySet
coCGate (memory, nameFlag, changeFlag) cbits m = (memory, nameFlag, False)
    where 
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ cGateMessage cbits ++ "\n"

coSendQMessage :: CoMemorySet -> [QBit] -> CoMemorySet
coSendQMessage (memory, nameFlag, changeFlag) qbits = (newMemory, not nameFlag, True)
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ sendQMessageMessage qbits ++ "\n"
        newMemory = putInQPocket memory qbits

coRecieveQMessage :: CoMemorySet -> ([QBit], CoMemorySet)
coRecieveQMessage (memory, nameFlag, changeFlag) = (memory, nameFlag, False)
    where
        newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ recieveQMessageMessage qbits ++ "\n"
        qbits = popFromQPocket memory

coSendCMessage :: CoMemorySet -> [CBit] -> CoMemorySet
coSendCMessage (memory, nameFlag, changeFlag) cbits = (newMemory, not nameFlag, True)
    where
       -- newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ sendCMessageMessage cbits ++ "\n"
        newMemory = putInCPocket memory cbits

coRecieveCMessage :: CoMemorySet -> ([CBit], CoMemorySet)
coRecieveCMessage (memory, nameFlag, changeFlag, message) = (memory, nameFlag, False)
    where
        --newMessage = message ++ introduce nameFlag changeFlag ++ tab ++ recieveCMessageMessage cbits ++ "\n"
        cbits = popFromCPocket memory-}
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
