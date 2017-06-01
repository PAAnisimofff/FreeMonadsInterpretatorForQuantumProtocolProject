{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logger.Comonad where

import System.IO
import Control.Monad.Free
import Control.Comonad.Cofree
import Lang.Lang
import Logger.Common
import Common.Matrix
import Common.Complex

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

-- собирает вместе монаду и комонаду
goTogether :: Cofree InterpreterF CoMemorySet
goTogether = coiter next emptyMemorySet
    where
        next w = InterpreterF (coQInit w) (coCInit w) (coMeasure w) (coQGate w) (coCGate w) (coSendQMessage w) (coRecieveQMessage w) (coSendCMessage w) (coRecieveCMessage w)

-- вызов комонадического интерпретатора
simplyLogCo :: Program () -> Program () -> IO ()
simplyLogCo progM progS = ioNew where
    (_, nameFlag, _, io) = pair const goTogether (mix progS progM)
    ioNew = io >> actorsOut nameFlag