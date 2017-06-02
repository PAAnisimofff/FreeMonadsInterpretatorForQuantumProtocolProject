module ExecutorTest where 
 
import Lang 
import Gates 
import AdHoc 
import Complex
executor :: Program () 
executor= do 
    qs <- qInit [15:+1.4, 2:+5, 34:+8, 2.6:+7.1, 2.8:+9.56, 5.8:+2.6] 
    c <- hadamard$ take 2 qs
    return () 
 
-- ad-hoc интерпретатор 
adhoc = testLog executor
 
-- комонадический интерпретатор 
--comonad = simplyLogCo alice bob