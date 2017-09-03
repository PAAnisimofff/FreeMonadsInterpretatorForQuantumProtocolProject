{-# LANGUAGE DeriveFunctor #-}
module Logger.Common where

import System.IO
-- import Control.Monad.Free
-- import Control.Comonad.Cofree
import Data.String.Utils (replace)
import Lang.Lang
import Common.Matrix
import Common.Complex
import System.Random
import Data.Foldable
import qualified Data.Vector as V
import qualified Data.Sequence as S

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
data QMemory = QMemConstr (S.Seq (Complex Double, Complex Double))
data CMemory = CMemConstr (S.Seq Bool)
data QPocket = QPoc [QBit]
data CPocket = CPoc [CBit]

data Memory = MemConstr QMemory CMemory QPocket CPocket

type CoMemorySet = (Memory, Bool, Bool)

emptyString = ""

emptyMemory = MemConstr (QMemConstr S.empty) (CMemConstr S.empty) (QPoc []) (CPoc [])

emptyMemorySet = (emptyMemory, True, True)

-- общие функции

getQMemoryList :: Memory -> S.Seq (Complex Double, Complex Double)
getQMemoryList (MemConstr (QMemConstr qmemory) _ _ _) = qmemory

getCMemoryList :: Memory -> S.Seq Bool
getCMemoryList (MemConstr _ (CMemConstr cmemory) _ _) = cmemory

setQMemoryList :: Memory -> S.Seq (Complex Double, Complex Double) -> Memory
setQMemoryList (MemConstr _ cm qp cp) list = MemConstr (QMemConstr list) cm qp cp

setCMemoryList :: Memory -> S.Seq Bool -> Memory
setCMemoryList (MemConstr qm _ qp cp) list = MemConstr qm (CMemConstr list) qp cp

addQBits :: Memory -> S.Seq (Complex Double, Complex Double) -> Memory
addQBits (MemConstr (QMemConstr qm) cm qp cp) income = MemConstr (QMemConstr (qm S.>< income)) cm qp cp
        

addCBits :: Memory -> S.Seq Bool -> Memory
addCBits (MemConstr qm (CMemConstr cm) qp cp) income = MemConstr qm (CMemConstr (cm S.>< income)) qp cp

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
boolToCD False = (0.0:+0.0, 0.0:+0.0)

double_to_seq :: (Complex Double, Complex Double) -> S.Seq (Complex Double)
double_to_seq (cd1, cd2) = S.fromList [cd1, cd2]

boolsToCDs :: S.Seq Bool -> S.Seq (Complex Double, Complex Double)
boolsToCDs bs = fmap boolToCD bs


measureQbit :: (Complex Double, Complex Double) -> IO ()
measureQbit qb = do
    rio <- randomRIO (0.0, 1.0)
    print $ useRnd qb rio

measureQbits :: S.Seq (Complex Double, Complex Double) -> IO ()
measureQbits qbs = do
    rios <- randomList (0.0, 1.0)
    print $ useRnds qbs (take (S.length qbs) rios)

------------Вариант выдающий на печать результат вместе со случайными числами
measureQbits' :: S.Seq (Complex Double, Complex Double) -> IO ()
measureQbits' qbs = do
    rios <- randomList (0.0, 1.0)
    print $ (useRnds' qbs (take (S.length qbs) rios), take (S.length qbs) rios)
-------------
randomList :: (Double, Double) -> IO [Double]
randomList range = getStdGen >>= return . randomRs range

useRnd :: (Complex Double, Complex Double) -> Double -> Bool
useRnd (a,b) r = if realPart ((abs a)^2) < r then True else False

useRnds :: S.Seq (Complex Double, Complex Double) -> [Double] -> Bool
useRnds cds rs = foldr1 (||) (S.zipWith useRnd cds (S.fromList rs))

--------Вариант без свертки
useRnds' :: S.Seq (Complex Double, Complex Double) -> [Double] -> [Bool]
useRnds' cds rs = toList $ S.zipWith useRnd cds (S.fromList rs)
--------

boo = boolsToCDs $ S.fromList [True, True, True, False] --Для тестов

--bl = S.update 2 True boo

adjustSome :: [Int] -> (a -> a) -> S.Seq a -> S.Seq a
adjustSome [] _ s1 = s1
adjustSome indices func s1 = adjustSome (tail indices) func (S.adjust func (head indices) s1)


throughGate :: Num a => (a, a) -> Matrix a -> (a, a)
throughGate (x, y) m = (new ! (1, 1), new ! (2, 1))
    where
        new = m * colVector (V.fromList [x, y])

updateMem :: Num a => S.Seq (a, a)
updateMem = adjustSome [0, 1] (`throughGate` (zero 2 2)) (S.fromList [(11,32), (43,54)])

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
        newMemory = addQBits memory (boolsToCDs $ S.fromList bits)

coCInit :: CoMemorySet -> [Bool] -> CoMemorySet
coCInit (memory, nameFlag, changeFlag) bits = (newMemory, nameFlag, False)
    where
        newMemory = addCBits memory (S.fromList bits)

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
