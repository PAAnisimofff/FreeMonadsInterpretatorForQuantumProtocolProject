module Lang where

import Control.Monad.Free

-- кубит
data QBit = QBitConstr Int

-- классический бит
data CBit = CBitConstr Int

-- набор команд
data Command rez = 
    QInit               Bool (QBit -> rez) |
    CInit               Bool (CBit -> rez) |
    Measure             QBit (QBit -> rez) |
    QGate               QBit (QBit -> rez) |
    SendQMessage        QBit          rez  |
    RecieveQMessage          (QBit -> rez) |
    SendCMessage        CBit          rez  |
    RecieveCMessage          (CBit -> rez)

-- которые функторы
instance Functor Command where
    fmap f (QInit           bit    g) = QInit        bit  (f . g)
    fmap f (CInit           bit    g) = CInit        bit  (f . g)
    fmap f (Measure         qbit   g) = Measure      qbit (f . g)
    fmap f (QGate           qbit   g) = QGate        qbit (f . g)
    fmap f (SendQMessage    qbit x  ) = SendQMessage qbit (f x)
    fmap f (RecieveQMessage        g) = RecieveQMessage   (f . g)
    fmap f (SendCMessage    cbit x  ) = SendCMessage cbit (f x)
    fmap f (RecieveCMessage        g) = RecieveCMessage   (f . g)

-- программа - список команд 
type Program = Free Command

-- для do-нотации
qInit :: Bool -> Program QBit
qInit bit = liftF (QInit bit id)

cInit :: Bool -> Program CBit
cInit bit = liftF (CInit bit id)

measure :: QBit -> Program QBit
measure qbit = liftF (Measure qbit id)

qGate :: QBit -> Program QBit
qGate qbit = liftF (QGate qbit id)

sendQMessage :: QBit -> Program ()
sendQMessage qbit = liftF (SendQMessage qbit ())

recieveQMessage :: Program QBit
recieveQMessage = liftF (RecieveQMessage id)

sendCMessage :: CBit -> Program ()
sendCMessage cbit = liftF (SendCMessage cbit ())

recieveCMessage :: Program CBit
recieveCMessage = liftF (RecieveCMessage id)