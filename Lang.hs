module Lang where

import Control.Monad.Free

-- кубит
data QBit = QBitConstr Int

-- классический бит
data CBit = CBitConstr Int

-- извлеечение номера бита
exractQNumber :: QBit -> Int
exractQNumber (QBitConstr qbitN) = qbitN

exractCNumber :: CBit -> Int
exractCNumber (CBitConstr cbitN) = cbitN

-- набор команд
data Command rez = 
    QInit               [Bool]        ([QBit] -> rez) |
    CInit               [Bool]        ([CBit] -> rez) |
    Measure             [QBit]        ([CBit] -> rez) |
    QGate               [QBit]        ([QBit] -> rez) |
    SendQMessage        [QBit]            rez  |
    RecieveQMessage                   ([QBit] -> rez) |
    SendCMessage        [CBit]            rez  |
    RecieveCMessage                   ([CBit] -> rez) |
    Hadamard            [QBit]        ([QBit] -> rez) |
    CNot                [QBit] [QBit] ([QBit] -> rez) |
    PauliX              [QBit]        ([QBit] -> rez) |
    PauliY              [QBit]        ([QBit] -> rez) |
    PauliZ              [QBit]        ([QBit] -> rez)


-- которые функторы
instance Functor Command where
    fmap f (QInit           bit          g) = QInit        bit        (f . g)
    fmap f (CInit           bit          g) = CInit        bit        (f . g)
    fmap f (Measure         qbit         g) = Measure      qbit       (f . g)
    fmap f (QGate           qbit         g) = QGate        qbit       (f . g)
    fmap f (SendQMessage    qbit       x  ) = SendQMessage qbit       (f x)
    fmap f (RecieveQMessage              g) = RecieveQMessage         (f . g)
    fmap f (SendCMessage    cbit       x  ) = SendCMessage cbit       (f x)
    fmap f (RecieveCMessage              g) = RecieveCMessage         (f . g)
    fmap f (Hadamard        qbit         g) = Hadamard     qbit       (f . g)
    fmap f (PauliX          qbit         g) = PauliX       qbit       (f . g)
    fmap f (PauliY          qbit         g) = PauliY       qbit       (f . g)
    fmap f (PauliZ          qbit         g) = PauliZ       qbit       (f . g)
    fmap f (CNot            qbitC qbit   g) = CNot         qbitC qbit (f . g)


-- программа - список команд 
type Program = Free Command

-- для do-нотации
qInit :: [Bool] -> Program [QBit]
qInit bit = liftF (QInit bit id)

cInit :: [Bool] -> Program [CBit]
cInit bit = liftF (CInit bit id)

measure :: [QBit] -> Program [CBit]
measure qbit = liftF (Measure qbit id)

qGate :: [QBit] -> Program [QBit]
qGate qbit = liftF (QGate qbit id)

sendQMessage :: [QBit] -> Program ()
sendQMessage qbit = liftF (SendQMessage qbit ())

recieveQMessage :: Program [QBit]
recieveQMessage = liftF (RecieveQMessage id)

sendCMessage :: [CBit] -> Program ()
sendCMessage cbit = liftF (SendCMessage cbit ())

recieveCMessage :: Program [CBit]
recieveCMessage = liftF (RecieveCMessage id)

hadamard :: [QBit] -> Program [QBit]
hadamard qbits = liftF (Hadamard qbits id)

pauliX :: [QBit] -> Program [QBit]
pauliX qbits = liftF (PauliX qbits id)

pauliY :: [QBit] -> Program [QBit]
pauliY qbits = liftF (PauliY qbits id)

pauliZ :: [QBit] -> Program [QBit]
pauliZ qbits = liftF (PauliZ qbits id)

cnot :: [QBit] -> [QBit] -> Program [QBit]
cnot qbitsF qbitsS = liftF (CNot qbitsF qbitsS id)