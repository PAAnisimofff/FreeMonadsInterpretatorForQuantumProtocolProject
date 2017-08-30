{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lang.Interpreters where

import Control.Monad.Free
import Control.Comonad.Cofree
import Lang.Lang

-- набор функторов интерпертатора
data InterpreterF rez = InterpreterF {
    qInitHandle           :: ([Bool]                     -> ([QBit], rez)),
    cInitHandle           :: ([Bool]                     -> ([CBit], rez)),
    measureHandle         :: ([QBit]                     -> ([CBit], rez)),
    qGateHandle           :: ([QBit] -> QGateDeterminant -> ([QBit], rez)),
    cGateHandle           :: ([CBit] -> СGateDeterminant -> ([CBit], rez)),
    sendQMessageHandle    ::  [QBit]                     ->          rez  ,
    recieveQMessageHandle ::                                ([QBit], rez) ,
    sendCMessageHandle    ::  [CBit]                     ->          rez  ,
    recieveCMessageHandle ::                                ([CBit], rez) 
} deriving Functor


-- ad-hoc интерпретатор

-- подробный вызов ad-hoc интерпертатора
adhocInterpreterRun :: (t -> InterpreterF t) -> t -> Program () -> Program () -> t
adhocInterpreterRun interpreter memory script1 script2 = let intr = interpreter memory in
    case script1 of
    Free (QInit bits next) -> adhocInterpreterRun interpreter newMemory (next qbits) script2 
        where
            (qbits, newMemory) = qInitHandle intr bits
    Free (CInit bits next) -> adhocInterpreterRun interpreter newMemory (next cbits) script2 
        where
            (cbits, newMemory) = cInitHandle intr bits
    Free (Measure qbits next) -> adhocInterpreterRun interpreter newMemory (next cbits) script2
        where
            (cbits, newMemory) = measureHandle intr qbits
    Free (QGate qbits m next) -> adhocInterpreterRun interpreter newMemory (next newQbits) script2
        where
            (newQbits, newMemory) = qGateHandle intr qbits m
    Free (CGate cbits f next) -> adhocInterpreterRun interpreter newMemory (next newCbits) script2
        where
            (newCbits, newMemory) = cGateHandle intr cbits f
    Free (SendQMessage qbits next) -> adhocInterpreterRun interpreter newMemory script2 (next) 
        where
            newMemory = sendQMessageHandle intr qbits
    Free (RecieveQMessage next) -> adhocInterpreterRun interpreter newMemory (next qbits) script2 
        where
            (qbits, newMemory) = recieveQMessageHandle intr
    Free (SendCMessage cbits next) -> adhocInterpreterRun interpreter newMemory script2 (next) 
        where
            newMemory = sendCMessageHandle intr cbits
    Free (RecieveCMessage next) -> adhocInterpreterRun interpreter newMemory (next cbits) script2 
        where
            (cbits, newMemory) = recieveCMessageHandle intr
    Pure r -> memory


-- комонадный интерпретатор

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

instance Pairing InterpreterF Command where
    pair f (InterpreterF qi _ _ _ _ _ _ _ _ ) (QInit           x   k) = pair f (qi x  )   k
    pair f (InterpreterF _ ci _ _ _ _ _ _ _ ) (CInit           x   k) = pair f (ci x  )   k
    pair f (InterpreterF _ _ m  _ _ _ _ _ _ ) (Measure         x   k) = pair f (m  x  )   k
    pair f (InterpreterF _ _ _ qg _ _ _ _ _ ) (QGate           x y k) = pair f (qg x y)   k
    pair f (InterpreterF _ _ _ _ cg _ _ _ _ ) (CGate           x y k) = pair f (cg x y)   k
    pair f (InterpreterF _ _ _ _ _ sq _ _ _ ) (SendQMessage    x   k) =      f (sq x  )   k
    pair f (InterpreterF _ _ _ _ _ _ rq _ _ ) (RecieveQMessage     k) = pair f  rq        k
    pair f (InterpreterF _ _ _ _ _ _ _ sc _ ) (SendCMessage    x   k) =      f (sc x  )   k
    pair f (InterpreterF _ _ _ _ _ _ _ _ rc ) (RecieveCMessage     k) = pair f  rc        k

-- подробный вызов комонадного интерпертатора
comonadInterpreterRun :: (t -> InterpreterF t) -> t -> Program () -> Program () -> t
comonadInterpreterRun interpreter memory script1 script2 = pair const tree script
    where
        script = mix script2 script1
        tree = coiter interpreter memory