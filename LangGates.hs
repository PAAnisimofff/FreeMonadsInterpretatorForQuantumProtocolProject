module LangGates where

import Lang
import Control.Monad.Free
import Matrix
import Complex

-- для списков
hadamardSingle :: QBit -> Program QBit
hadamardSingle qbit = singler . liftF $ QGate [qbit] m id 
    where
        mSize = 2
        m = matrix mSize mSize (\ (x,y) -> 0 :+ 0)

pauliXSingle :: QBit -> Program QBit
pauliXSingle qbit = singler . liftF $ QGate [qbit] m id 
    where
        mSize = 2
        m = matrix mSize mSize (\ (x,y) -> 0 :+ 0)

pauliYSingle :: QBit -> Program QBit
pauliYSingle qbit = singler . liftF $ QGate [qbit] m id 
    where
        mSize = 2
        m = matrix mSize mSize (\ (x,y) -> 0 :+ 0)

pauliZSingle :: QBit -> Program QBit
pauliZSingle qbit = singler . liftF $ QGate [qbit] m id 
    where
        mSize = 2
        m = matrix mSize mSize (\ (x,y) -> 0 :+ 0)

cnotSingle :: QBit -> QBit -> Program QBit
cnotSingle qbitF qbitS = singler . liftF $ QGate [qbitF, qbitS] m id 
    where
        mSize = 2 ^ 2
        m = matrix mSize mSize (\ (x,y) -> 0 :+ 0)

-- для одиночных битов
hadamard :: [QBit] -> Program [QBit]
hadamard qbits = liftF $ QGate qbits m id 
    where
        mSize = 2 ^ length qbits
        m = matrix mSize mSize (\ (x,y) -> 0 :+ 0)

pauliX :: [QBit] -> Program [QBit]
pauliX qbits = liftF $ QGate qbits m id 
    where
        mSize = 2 ^ length qbits
        m = matrix mSize mSize (\ (x,y) -> 0 :+ 0)

pauliY :: [QBit] -> Program [QBit]
pauliY qbits = liftF $ QGate qbits m id 
    where
        mSize = 2 ^ length qbits
        m = matrix mSize mSize (\ (x,y) -> 0 :+ 0)

pauliZ :: [QBit] -> Program [QBit]
pauliZ qbits = liftF $ QGate qbits m id 
    where
        mSize = 2 ^ length qbits
        m = matrix mSize mSize (\ (x,y) -> 0 :+ 0)

cnot :: [QBit] -> [QBit] -> Program [QBit]
cnot qbitsF qbitsS = liftF $ QGate (qbitsF ++ qbitsS) m id 
    where
        mSize = 2 ^ (length qbitsF + length qbitsS) 
        m = matrix mSize mSize (\ (x,y) -> 0 :+ 0)