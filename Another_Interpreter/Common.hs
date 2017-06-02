module Common where

import System.IO
import Data.String.Utils (replace)
import Lang
import Matrix
import Complex
import qualified Data.Vector as V

-- память
data QMemory = QMemConstr [Complex Double]
data QPocket = QPoc [QBit]

data Memory = MemConstr QMemory QPocket 

emptyMemory = MemConstr (QMemConstr []) (QPoc []) 


-- общие функции

getQMemoryList :: Memory -> [Complex Double]
getQMemoryList (MemConstr (QMemConstr qmemory) _) = qmemory


setQMemoryList :: Memory -> [Complex Double] -> Memory
setQMemoryList (MemConstr _ qp) list = MemConstr (QMemConstr list) qp


addQBits :: Memory -> [Complex Double] -> Memory
addQBits (MemConstr (QMemConstr qm) qp) n = let new = V.toList $ getCol 1 tMat in
        MemConstr (QMemConstr new) qp
        where tMat = tensor (fromList (length qm) 1 qm) (fromList (length n) 1 n)

throwQBits :: Memory -> [Complex Double] -> Memory
throwQBits (MemConstr (QMemConstr qm) qp) n = let new = n in
        MemConstr (QMemConstr (new ++ qm)) qp


endOrZero :: [Complex Double] -> Complex Double
endOrZero [] = 0 :+ 0
endOrZero list  = last list 

putInQPocket :: Memory -> [QBit] -> Memory
putInQPocket (MemConstr qm _) x =  MemConstr qm (QPoc x)

popFromQPocket :: Memory -> [QBit]
popFromQPocket (MemConstr qm (QPoc x)) = x


showQBits :: [QBit] -> String
showQBits qbits = show $ map exractQNumber qbits

gMatrix :: [QBit] -> Matrix (Complex Double)
gMatrix q2 = fromList (length q2) 1 (map exractQNumber q2)


-- сообщения

tab :: IO ()
tab = putStr "\t"

greetings :: IO ()
greetings = putStrLn "Commence calculation. Data sent."

qGatePassedMessage :: [QBit] -> IO ()
qGatePassedMessage qbits = putStrLn $ "Now it looks like this: " ++ showQBits qbits ++ "."

qInitMessage :: [QBit] -> IO ()
qInitMessage qbitNs = putStrLn $ "- Show off input probability amplitudes " ++ showQBits qbitNs ++ "."

qGateMessage :: [QBit] -> (Matrix (Complex Double)) -> IO ()
qGateMessage qbits m = putStrLn $ "- Qubits " ++ showQBits qbits ++ " went through the gate with matrix:\n\n\t" ++ (replace "\n" "\n\t" (show m))
