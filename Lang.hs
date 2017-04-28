{-# LANGUAGE DeriveFunctor #-}

module Lang where

import Control.Monad.Free

-- кубит
data QBit = QBit Int
    deriving (Eq, Show)

-- классический бит
data CBit = CBit Int
    deriving (Eq, Show)

-- извлеечение номера бита
exractQNumber :: QBit -> Int
exractQNumber (QBit qbitN) = qbitN

exractCNumber :: CBit -> Int
exractCNumber (CBit cbitN) = cbitN

-- набор команд
data Command rez = 
    QInit           [Bool] ([QBit] -> rez) |
    CInit           [Bool] ([CBit] -> rez) |
    Measure         [QBit] ([CBit] -> rez) |
    QGate           [QBit] ([QBit] -> rez) |
    SendQMessage    [QBit]            rez  |
    RecieveQMessage        ([QBit] -> rez) |
    SendCMessage    [CBit]            rez  |
    RecieveCMessage        ([CBit] -> rez)
    deriving Functor

-- программа - список команд 
type Program = Free Command

-- превращает возвращаемый функтором список в одиночный элемент
singler :: (Functor f) => f [r] -> f r
singler command = fmap (\ [x] -> x) command


-- функции-обёртки

-- для списков
qInit :: [Bool] -> Program [QBit]
qInit bits = liftF $ QInit bits id

cInit :: [Bool] -> Program [CBit]
cInit bits = liftF $ CInit bits id

measure :: [QBit] -> Program [CBit]
measure qbits = liftF $ Measure qbits id

sendQMessage :: [QBit] -> Program ()
sendQMessage qbits = liftF $ SendQMessage qbits ()

recieveQMessage :: Program [QBit]
recieveQMessage = liftF $ RecieveQMessage id

sendCMessage :: [CBit] -> Program ()
sendCMessage cbits = liftF $ SendCMessage cbits ()

recieveCMessage :: Program [CBit]
recieveCMessage = liftF $ RecieveCMessage id

hadamard :: [QBit] -> Program [QBit]
hadamard qbits = liftF $ QGate qbits id

pauliX :: [QBit] -> Program [QBit]
pauliX qbits = liftF $ QGate qbits id

pauliY :: [QBit] -> Program [QBit]
pauliY qbits = liftF $ QGate qbits id

pauliZ :: [QBit] -> Program [QBit]
pauliZ qbits = liftF $ QGate qbits id

cnot :: [QBit] -> [QBit] -> Program [QBit]
cnot qbitsF qbitsS = liftF $ QGate (qbitsF ++ qbitsS) id

-- для одиночных битов
qInitSingle :: Bool -> Program QBit
qInitSingle bit = singler . liftF $ QInit [bit] id

cInitSingle :: Bool -> Program CBit
cInitSingle bit = singler . liftF $ CInit [bit] id

measureSingle :: QBit -> Program CBit
measureSingle qbit = singler . liftF $ Measure [qbit] id

sendQMessageSingle :: QBit -> Program ()
sendQMessageSingle qbit = liftF $ SendQMessage [qbit] ()

recieveQMessageSingle :: Program QBit
recieveQMessageSingle = singler . liftF $ RecieveQMessage id

sendCMessageSingle :: CBit -> Program ()
sendCMessageSingle cbit = liftF $ SendCMessage [cbit] ()

recieveCMessageSingle :: Program CBit
recieveCMessageSingle = singler . liftF $ RecieveCMessage id

hadamardSingle :: QBit -> Program QBit
hadamardSingle qbit = singler . liftF $ QGate [qbit] id

pauliXSingle :: QBit -> Program QBit
pauliXSingle qbit = singler . liftF $ QGate [qbit] id

pauliYSingle :: QBit -> Program QBit
pauliYSingle qbit = singler . liftF $ QGate [qbit] id

pauliZSingle :: QBit -> Program QBit
pauliZSingle qbit = singler . liftF $ QGate [qbit] id

cnotSingle :: QBit -> QBit -> Program QBit
cnotSingle qbitF qbitS = singler . liftF $ QGate [qbitF, qbitS] id