module Gates where

import Lang
import Control.Monad.Free
import Matrix
import Complex

-- для списков
hadamardSingle :: QBit -> Program QBit
hadamardSingle qbit = singler . liftF $ QGate [qbit] m id 
    where
        mSize = 2
        m = scaleMatrix (1/sqrt 2) (matrix mSize mSize (\(i,j) -> if i==mSize && j==mSize then (-1) :+ 0 else 1 :+ 0))

pauliXSingle :: QBit -> Program QBit
pauliXSingle qbit = singler . liftF $ QGate [qbit] m id 
    where
        mSize = 2
        m = matrix mSize mSize (\(i,j) -> if i==j then 0 :+ 0 else 1 :+ 0)

pauliYSingle :: QBit -> Program QBit
pauliYSingle qbit = singler . liftF $ QGate [qbit] m id 
    where
        mSize = 2
        m = matrix mSize mSize (\ (i,j) -> if i==j then 0 :+ 0 else if i==1 && j==2 then 0:+(-1) else 0:+1)

pauliZSingle :: QBit -> Program QBit
pauliZSingle qbit = singler . liftF $ QGate [qbit] m id 
    where
        mSize = 2
        m = matrix mSize mSize (\ (i,j) -> if i/=j then 0 :+ 0 else if i==1 && j==1 then 1:+0 else (-1):+0)

cnotSingle :: QBit -> QBit -> Program QBit
cnotSingle qbitF qbitS = singler . liftF $ QGate [qbitF, qbitS] m id 
    where
        mSize = 2 
        m = permMatrixC mSize (mSize-1) mSize

-- для одиночных битов
hadamard :: [QBit] -> Program [QBit]
hadamard qbits = liftF $ QGate qbits m id 
    where
        mSize = length qbits
        m = scaleMatrix (1/sqrt 2) (matrix mSize mSize (\(i,j) -> if i==mSize && j==mSize then (-1) :+ 0 else 1 :+ 0))

pauliX :: [QBit] -> Program [QBit]
pauliX qbits = liftF $ QGate qbits m id 
    where
        mSize = length qbits
        m = matrix mSize mSize (\(i,j) -> if i==j then 0 :+ 0 else 1 :+ 0)

pauliY :: [QBit] -> Program [QBit]
pauliY qbits = liftF $ QGate qbits m id 
    where
        mSize = length qbits
        m = matrix mSize mSize (\ (i,j) -> if i==j then 0 :+ 0 else if i==1 && j==2 then 0:+(-1) else 0:+1)

pauliZ :: [QBit] -> Program [QBit]
pauliZ qbits = liftF $ QGate qbits m id 
    where
        mSize = length qbits
        m = matrix mSize mSize (\ (i,j) -> if i/=j then 0 :+ 0 else if i==1 && j==1 then 1:+0 else (-1):+0)

cnot :: [QBit] -> [QBit] -> Program [QBit]
cnot qbitsF qbitsS = liftF $ QGate (qbitsF ++ qbitsS) m id 
    where
        mSize = 2 ^ (length qbitsF + length qbitsS) 
        m = permMatrixC mSize (mSize-1) mSize