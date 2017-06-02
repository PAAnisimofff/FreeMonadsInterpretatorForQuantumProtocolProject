module AdHoc where

import System.IO
import Control.Monad.Free
import Lang
import Common
import Complex
import Matrix


-- ad-hoc интерпретатор
commandlog :: Memory -> Program () -> IO ()
commandlog memory prog = 
    case prog of
    Free (QInit newCD next) -> do
           greetings
           tab
           qInitMessage qbits
           commandlog newMemory (next qbits)
           where
               newMemory = throwQBits memory newCD
               qbits = map QBit newCD
    Free (QGate qbits m next) -> do
        putStr "Passing gate"
        tab
        qGateMessage qbits m
        qGatePassedMessage newQbits
        commandlog newMemory (next newQbits) where
            newMemory = addQBits memory (map exractQNumber newQbits)
            newQbits = map QBit (matTolist $ multStrassenMixed m (gMatrix qbits)) --берем 2 кубита, превращаем в матрицу, умножаем на гейт
    Pure r -> do
        putStr "That's it\n"
        return r

  

-- вызов ad-hoc интепретатора
testLog :: Program () -> IO ()
testLog calc = commandlog emptyMemory calc