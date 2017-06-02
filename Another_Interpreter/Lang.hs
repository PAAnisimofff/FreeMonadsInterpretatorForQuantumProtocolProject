{-# LANGUAGE DeriveFunctor #-}

module Lang where

import Control.Monad.Free
import Matrix
import Complex

-- кубит
data QBit = QBit (Complex Double)
    deriving (Eq, Show)

-- извлеечение номера бита
exractQNumber :: QBit -> Complex Double
exractQNumber (QBit qbitN) = qbitN


-- набор команд
data Command rez = 
    QInit           [Complex Double]                         ([QBit] -> rez) |
    QGate           [QBit] (Matrix (Complex Double)) ([QBit] -> rez) 
    deriving Functor

-- программа - список команд 
type Program = Free Command

-- превращает возвращаемый функтором список в одиночный элемент
singler :: (Functor f) => f [r] -> f r
singler command = fmap (\ [x] -> x) command


-- функции-обёртки

-- для списков
qInit :: [Complex Double] -> Program [QBit]
qInit bits = liftF $ QInit bits id

qInitSingle :: Complex Double -> Program QBit
qInitSingle bit = singler . liftF $ QInit [bit] id


