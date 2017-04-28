{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LoggerComonad where

import System.IO
import Control.Monad.Free
import Control.Comonad.Cofree
import Lang
import LoggerCommon

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
    recieveCMessageHandle ::                      ([CBit], rez) 
} deriving Functor

instance Pairing InterpreterF Command where
    pair f (InterpreterF qi _ _ _ _ _ _ _ ) (QInit           x   k) = pair f (qi x)   k
    pair f (InterpreterF _ ci _ _ _ _ _ _ ) (CInit           x   k) = pair f (ci x)   k
    pair f (InterpreterF _ _ m  _ _ _ _ _ ) (Measure         x   k) = pair f (m  x)   k
    pair f (InterpreterF _ _ _ qg _ _ _ _ ) (QGate           x   k) = pair f (qg x)   k
    pair f (InterpreterF _ _ _ _ sq _ _ _ ) (SendQMessage    x   k) =      f (sq x)   k
    pair f (InterpreterF _ _ _ _ _ rq _ _ ) (RecieveQMessage     k) = pair f  rq      k
    pair f (InterpreterF _ _ _ _ _ _ sc _ ) (SendCMessage    x   k) =      f (sc x)   k
    pair f (InterpreterF _ _ _ _ _ _ _ rc ) (RecieveCMessage     k) = pair f  rc      k

type CoProgram a = Cofree InterpreterF a

-- набор памяти
type CoMemorySet = (Memory, Bool, Bool, IO ())

-- собирает вместе монаду и комонаду
goTogether :: CoProgram CoMemorySet
goTogether = coiter next start
    where
        next w = InterpreterF (coQInit w) (coCInit w) (coMeasure w) (coQGate w) (coSendQMessage w) (coRecieveQMessage w) (coSendCMessage w) (coRecieveCMessage w)
        start = (emptyMemory, True, True, return ())


-- реализация обработчиков комонадического интерпретатора

coQInit :: CoMemorySet -> [Bool] -> ([QBit], CoMemorySet)
coQInit (memory, nameFlag, changeFlag, io) bits = (qbits, (newMemory, nameFlag, False, newio)) 
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> qInitMessage qbits
        (qbitNs, newMemory) = addQBits memory (length bits)
        qbits = map QBit qbitNs

coCInit :: CoMemorySet -> [Bool] -> ([CBit], CoMemorySet)
coCInit (memory, nameFlag, changeFlag, io) bits = (cbits, (newMemory, nameFlag, False, newio)) 
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> cInitMessage cbits
        (cbitNs, newMemory) = addCBits memory (length bits)
        cbits = map CBit cbitNs

coMeasure :: CoMemorySet -> [QBit] -> ([CBit], CoMemorySet)
coMeasure (memory, nameFlag, changeFlag, io) qbits = (cbits, (newMemory, nameFlag, False, newio))
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> measureMessage qbits cbits
        (cbitNs, newMemory) = addCBits memory (length qbits)
        cbits = map CBit cbitNs

coQGate :: CoMemorySet -> [QBit] -> ([QBit], CoMemorySet)
coQGate (memory, nameFlag, changeFlag, io) qbits = (qbits, (memory, nameFlag, False, newio))
    where 
        newio = io >> introduce nameFlag changeFlag >> tab >> qAbstractGateMessage qbits

coSendQMessage :: CoMemorySet -> [QBit] -> CoMemorySet
coSendQMessage (memory, nameFlag, changeFlag, io) qbits = (newMemory, not nameFlag, True,  newio)
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> sendQMessageMessage qbits
        newMemory = putInQPocket memory qbits

coRecieveQMessage :: CoMemorySet -> ([QBit], CoMemorySet)
coRecieveQMessage (memory, nameFlag, changeFlag, io) = (qbits, (memory, nameFlag, False, newio))
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> recieveQMessageMessage qbits
        qbits = popFromQPocket memory

coSendCMessage :: CoMemorySet -> [CBit] -> CoMemorySet
coSendCMessage (memory, nameFlag, changeFlag, io) cbits = (newMemory, not nameFlag, True,  newio)
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> sendCMessageMessage cbits
        newMemory = putInCPocket memory cbits

coRecieveCMessage :: CoMemorySet -> ([CBit], CoMemorySet)
coRecieveCMessage (memory, nameFlag, changeFlag, io) = (cbits, (memory, nameFlag, False, newio))
    where
        newio = io >> introduce nameFlag changeFlag >> tab >> recieveCMessageMessage cbits
        cbits = popFromCPocket memory


-- вызов комонадического интерпретатора
simplyLogCo :: Program () -> Program () -> IO ()
simplyLogCo progM progS = ioNew where
    (_, nameFlag, _, io) = pair const goTogether (mix progS progM)
    ioNew = io >> actorsOut nameFlag