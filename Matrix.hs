
-- | Операции над матрицами.
 
module Matrix (
    -- * Типы матриц
    Matrix , prettyMatrix
  , nrows , ncols
  , forceMatrix
    -- * конструкторы
  , matrix
  , fromList , fromLists
  , rowVector
  , colVector
    -- ** Специальные матрицы
  , zero
  , identity
  , permMatrix
    -- * Доступ к элементам
  , getElem , (!)
  , getRow  , getCol
  , getDiag
    -- * Манипуляции над матрицами
  , setElem
  , transpose , extendTo
  , mapRow
    -- * Подматрицы
    -- ** Разделение на блоки
  , submatrix
  , minorMatrix
  , splitBlocks
    -- ** Соединение блоков
  , (<|>) , (<->)
  , joinBlocks
    -- * Умножения матриц
  , multStd
  , multStrassen
  , multStrassenMixed
    -- * линейные преобразования
  , scaleMatrix
  , scaleRow
  , combineRows
  , switchRows
    -- * свойства
  , trace , diagProd
    -- ** детерминанты
  , detLaplace
  ) where

import Data.Monoid
import Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.Primitive (PrimMonad,PrimState)
import Data.List (maximumBy)
import Complex
-------------------------------------------------------
-------------------------------------------------------
---- ТИП МАТРИЦ

-- | Тип
data Matrix a = M {
   nrows :: !Int -- ^ число строк.
 , ncols :: !Int -- ^ число столбцов.
 , mvect ::  V.Vector (V.Vector a)
   } deriving Eq


sizeStr :: Int -> Int -> String
sizeStr n m = show n ++ "x" ++ show m

-- | отображение матрицы в виде строки с использованием Show (фигня, не использовать)
prettyMatrix :: Show a => Matrix a -> String
prettyMatrix m@(M _ _ v) = unlines
 [ "( " <> unwords (fmap (\j -> fill mx $ show $ m ! (i,j)) [1..ncols m]) <> " )" | i <- [1..nrows m] ]
 where
  mx = V.maximum $ fmap (V.maximum . fmap (length . show)) v
  fill k str = replicate (k - length str) ' ' ++ str

instance Show a => Show (Matrix a) where
 show = prettyMatrix

instance NFData a => NFData (Matrix a) where
 rnf (M _ _ v) = rnf v

-- | /O(rows*cols)/. Убирает ссылки на исходную матрицу повзоляя включится уборщику мусора(?).
--
--   .
forceMatrix :: Matrix a -> Matrix a
forceMatrix (M n m v) = M n m $ V.map V.force $ V.force v

-------------------------------------------------------
-------------------------------------------------------
---- Конструкторы

-- | /O(rows*cols)/. нулевая матрица данного размера.
--
-- > zero n m =
-- >                 n
-- >   1 ( 0 0 ... 0 0 )
-- >   2 ( 0 0 ... 0 0 )
-- >     (     ...     )
-- >     ( 0 0 ... 0 0 )
-- >   n ( 0 0 ... 0 0 )
zero :: Num a =>
     Int -- ^ строки
  -> Int -- ^ столбцы
  -> Matrix a
zero n m = M n m $ V.replicate n $ V.replicate m 0

-- | /O(rows*cols)/. создать матрицу используя генературную функцию зависящую от индексов
--   Пример:
--
-- >                                  (  1  0 -1 -2 )
-- >                                  (  3  2  1  0 )
-- >                                  (  5  4  3  2 )
-- > matrix 4 4 $ \(i,j) -> 2*i - j = (  7  6  5  4 )
matrix :: Int -- ^ строки
       -> Int -- ^ столбцы
       -> ((Int,Int) -> a) -- ^ генераторная функция
       -> Matrix a
matrix n m f = M n m $ V.generate n $ \i -> V.generate m $ \j -> f (i+1,j+1)

-- | /O(rows*cols)/. Еденичная матрица.
--
-- > identity n =
-- >                 n
-- >   1 ( 1 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 0 )
-- >     (     ...     )
-- >     ( 0 0 ... 1 0 )
-- >   n ( 0 0 ... 0 1 )
--
identity :: Num a => Int -> Matrix a
identity n = matrix n n $ \(i,j) -> if i == j then 1 else 0

-- |Создает матрицу из непустого списка.
--  В списке нужно хотя бы m x n элементов.
--  Пример:
--
-- >                       ( 1 2 3 )
-- >                       ( 4 5 6 )
-- > fromList 3 3 [1..] =  ( 7 8 9 )
--
fromList :: Int -- ^ строки
         -> Int -- ^ столбцы
         -> [a] -- ^ Список
         -> Matrix a
fromList n m xs = fromLists $ go 1 xs
 where
  go i ys = if i > n
               then []
               else let (r,zs) = splitAt m ys
                    in  r : go (succ i) zs

-- | Создает матрицу из списка списков (смотри пример).
--   Все списки должны быть непустые.
--   Пример:
--
-- > fromLists [ [1,2,3]      ( 1 2 3 )
-- >           , [4,5,6]      ( 4 5 6 )
-- >           , [7,8,9] ] =  ( 7 8 9 )
--
fromLists :: [[a]] -> Matrix a
-- 
fromLists xss = M (length xss) (length $ head xss) $ V.fromList $ fmap V.fromList xss

-- | /O(1)/. Переделывает элемент из Data.Vector в вектор-строку Matrix.
rowVector :: V.Vector a -> Matrix a
rowVector v = M 1 (V.length v) $ V.singleton v

-- | /O(rows)/. Переделывает элемент из Data.Vector в вектор-столбец Matrix.
colVector :: V.Vector a -> Matrix a
colVector v = M (V.length v) 1 $ V.map V.singleton v

-- | /O(rows*cols)/. Матрица перестановки.
--
-- > permMatrix n i j =
-- >               i     j       n
-- >   1 ( 1 0 ... 0 ... 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   i ( 0 0 ... 0 ... 1 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   j ( 0 0 ... 1 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >     ( 0 0 ... 0 ... 0 ... 1 0 )
-- >   n ( 0 0 ... 0 ... 0 ... 0 1 )
--
-- 
--
permMatrix :: Num a
           => Int -- ^ Размер
           -> Int -- ^ столбец перестановки 1.
           -> Int -- ^ столбец перестановки 2.
           -> Matrix a 
permMatrix n r1 r2 | r1 == r2 = identity n
permMatrix n r1 r2 = matrix n n f
 where
  f (i,j)
   | i == r1 = if j == r2 then 1 else 0
   | i == r2 = if j == r1 then 1 else 0
   | i == j = 1
   | otherwise = 0

-------------------------------------------------------
-------------------------------------------------------
---- ДОСТУП

-- | /O(1)/. Взять указанный элемент.
getElem :: Int      -- ^ строка
        -> Int      -- ^ столбец
        -> Matrix a -- ^ матрица
        -> a
getElem i j (M n m v)
 | i > n || j > m = error $ "Trying to get the " ++ show (i,j) ++ " element from a "
                         ++ sizeStr n m ++ " matrix."
 | otherwise = (v V.! (i-1)) V.! (j-1)

-- | оператор под взятие.
(!) :: Matrix a -> (Int,Int) -> a
m ! (i,j) = getElem i j m

-- | /O(1)/. взять строку как вектор.
getRow :: Int -> Matrix a -> V.Vector a
getRow i (M _ _ vs) = vs V.! (i-1)

-- | /O(rows)/. Взять столбец как вектор.
getCol :: Int -> Matrix a -> V.Vector a
getCol j a@(M n _ _) = V.generate n $ \i -> a ! (i+1,j)

-- | /O(min rows cols)/. взять диагональ как вектор.
getDiag :: Matrix a -> V.Vector a
getDiag m = V.generate k $ \i -> m ! (i+1,i+1)
 where
  k = min (nrows m) (ncols m)

-------------------------------------------------------
-------------------------------------------------------
---- МАНИПУЛИРОВАНИЕ МАТРИЦАМИ

msetElem:: PrimMonad m => a -> (Int,Int) -> MV.MVector (PrimState m) (V.Vector a) -> m ()
msetElem x (i,j) m = do
 r <- MV.read m (i-1)
 MV.write m (i-1) $ V.modify (\mv -> MV.write mv (j-1) x) r

-- | /O(1)/. Заменить элемент в матрице.
setElem :: a -- ^ новое значениеe.
        -> (Int,Int) -- ^ позиция.
        -> Matrix a 
        -> Matrix a  
setElem x p (M n m vs) = M n m $ V.modify (msetElem x p) vs

-- | /O(rows*cols)/. Транспонирование.
--   Пример:
--
-- >           ( 1 2 3 )   ( 1 4 7 )
-- >           ( 4 5 6 )   ( 2 5 8 )
-- > transpose ( 7 8 9 ) = ( 3 6 9 )
transpose :: Matrix a -> Matrix a
transpose m = matrix (ncols m) (nrows m) $ \(i,j) -> m ! (j,i)

-- | увеличить матрицу добавив нули.
--   Пример:
--
-- >                          ( 1 2 3 0 0 )
-- >              ( 1 2 3 )   ( 4 5 6 0 0 )
-- >              ( 4 5 6 )   ( 7 8 9 0 0 )
-- > extendTo 4 5 ( 7 8 9 ) = ( 0 0 0 0 0 )
extendTo :: Num a
         => Int -- ^ новое кол-во строк.
         -> Int -- ^ новое кол-во столбцов.
         -> Matrix a -> Matrix a
extendTo n m a = a''
 where
  n'  = n - nrows a
  a'  = if n' <= 0 then a  else a  <-> zero n' (ncols a)
  m'  = m - ncols a
  a'' = if m' <= 0 then a' else a' <|> zero (nrows a') m'

-------------------------------------------------------
-------------------------------------------------------
---- РАБОТА СБЛОКАМИ

-- | /O(r2-r1)/. вЫТАЩИТЬ ПОДМАТРИЦУ.
--   Example:
--
-- >                   ( 1 2 3 )
-- >                   ( 4 5 6 )   ( 2 3 )
-- > submatrix 1 2 2 3 ( 7 8 9 ) = ( 5 6 )
submatrix :: Int    -- ^ НАЧАЛЬНАЯ СТРОКА /r1/
             -> Int -- ^ конечная строка /r2/
          -> Int    -- ^ начальный столбец
             -> Int -- ^ конечный столбец
          -> Matrix a
          -> Matrix a
{-# INLINE submatrix #-}
submatrix r1 r2 c1 c2 (M _ _ vs) = M r' c' $ V.map (V.unsafeSlice (c1-1) c') $ V.unsafeSlice (r1-1) r' vs
  where
   r' = r2-r1+1
   c' = c2-c1+1

-- | удалить строку и столбец из матрицы.
--   Example:
--
-- >                 ( 1 2 3 )
-- >                 ( 4 5 6 )   ( 1 3 )
-- > minorMatrix 2 2 ( 7 8 9 ) = ( 7 9 )
minorMatrix :: Int -- ^ строка r к удалению.
            -> Int -- ^ столбец c к удалению.
            -> Matrix a -- ^ Original matrix.
            -> Matrix a 
minorMatrix r c (M n m v) = M (n-1) (m-1) $
  V.map (V.ifilter $ \j _ -> j+1 /= c) $
    V.ifilter (\i _ -> i+1 /= r) v

-- |расщепить матрицу на 4 блока используя данный элемент как координату
--
--Рисунок ->
--
-- >                 (             )   (      |      )
-- >                 (             )   ( ...  | ...  )
-- >                 (    x        )   (    x |      )
-- > splitBlocks i j (             ) = (-------------) , где x = a_{i,j}
-- >                 (             )   (      |      )
-- >                 (             )   ( ...  | ...  )
-- >                 (             )   (      |      )
-- > ( TL | TR )
-- > (---------)
-- > ( BL | BR )
--
--
splitBlocks :: Int      
            -> Int     
            -> Matrix a
            -> (Matrix a,Matrix a
               ,Matrix a,Matrix a)
{-# INLINE splitBlocks #-}
splitBlocks i j a@(M n m _) = ( submatrix    1  i 1 j a , submatrix    1  i (j+1) m a
                              , submatrix (i+1) n 1 j a , submatrix (i+1) n (j+1) m a )

-- | воссединиить куски обратно.
joinBlocks :: (Matrix a,Matrix a
              ,Matrix a,Matrix a)
           ->  Matrix a
{-# INLINE joinBlocks #-}
joinBlocks (tl,tr,bl,br) = (tl <|> tr)
                               <->
                           (bl <|> br)

-- | горизонтально сцепить 2 матрицы
--
-- > ( A ) <|> ( B ) = ( A | B )
--
(<|>) :: Matrix a -> Matrix a -> Matrix a
{-# INLINE (<|>) #-}
(M n m vs) <|> (M n' m' vs')
 | n /= n' = error $ "Horizontal join of " ++ sizeStr n m ++ " and "
                  ++ sizeStr n' m' ++ " matrices."
 | otherwise = M n (m+m') $ V.zipWith (V.++) vs vs'

-- | вертикально сцепить 2 матрицы
--
-- >                   ( A )
-- > ( A ) <-> ( B ) = ( - )
-- >                   ( B )
--
(<->) :: Matrix a -> Matrix a -> Matrix a
{-# INLINE (<->) #-}
(M n m v) <-> (M n' m' v')
 | m /= m' = error $ "Vertical join of " ++ sizeStr n m ++ " and "
                  ++ sizeStr n' m' ++ " matrices."
 | otherwise = M (n+n') m $ v V.++ v'

-------------------------------------------------------
-------------------------------------------------------
---- УМНОЖЕНИЕ МАТРИЦ

-- | Стандартное умножение.
multStd :: Num a => Matrix a -> Matrix a -> Matrix a
multStd a1@(M n m _) a2@(M n' m' _)
   -- Проверка размеров
   | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and "
                    ++ sizeStr n' m' ++ " matrices."
   | otherwise = multStd_ a1 a2

-- | без проверки размеров (не испольозовать)
multStd_ :: Num a => Matrix a -> Matrix a -> Matrix a
multStd_ a1@(M n m _) a2@(M _ m' _) = matrix n m' $ \(i,j) -> sum [ a1 ! (i,k) * a2 ! (k,j) | k <- [1 .. m] ]

first :: (a -> Bool) -> [a] -> a
first f = go
 where
  go (x:xs) = if f x then x else go xs
  go [] = error "first: no element match the condition."

-- | Алгоритм Штрассена для матриц порядка 2^n.
strassen :: Num a => Matrix a -> Matrix a -> Matrix a
-- тривиальное 1х1.
strassen (M 1 1 v) (M 1  1  v') = M 1 1 $ V.zipWith (V.zipWith (*)) v v'
-- матрицы должны быть квадратными
strassen a b = joinBlocks (c11,c12,c21,c22)
 where
  n = div (nrows a) 2
  (a11,a12,a21,a22) = splitBlocks n n a
  (b11,b12,b21,b22) = splitBlocks n n b

  p1 = strassen (a11 + a22) (b11 + b22)
  p2 = strassen (a21 + a22)  b11
  p3 = strassen  a11        (b12 - b22)
  p4 = strassen        a22  (b21 - b11)
  p5 = strassen (a11 + a12)        b22
  p6 = strassen (a21 - a11) (b11 + b12)
  p7 = strassen (a12 - a22) (b21 + b22)

  c11 = p1 + p4 - p5 + p7
  c12 = p3 + p5
  c21 = p2 + p4
  c22 = p1 - p2 + p3 + p6

-- | Умножение по Штрассену (только для порядка 2^n) .
multStrassen :: Num a => Matrix a -> Matrix a -> Matrix a
multStrassen a1@(M n m _) a2@(M n' m' _)
   | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and "
                    ++ sizeStr n' m' ++ " matrices."
   | otherwise =
       let mx = maximum [n,m,n',m']
           n2  = first (>= mx) $ fmap (2^) [(0 :: Int)..]
           b1 = extendTo n2 n2 a1
           b2 = extendTo n2 n2 a2
       in  submatrix 1 n 1 m' $ strassen b1 b2

strmixFactor :: Int
strmixFactor = 75

-- | Смешаный алгоритм Штрассена
-- Под него сделан оператор (*) в Instance для Num
strassenMixed :: Num a => Matrix a -> Matrix a -> Matrix a
strassenMixed a@(M r _ _) b
 | r < strmixFactor = multStd_ a b
 | odd r = let r' = r + 1
               a' = extendTo r' r' a
               b' = extendTo r' r' b
           in  submatrix 1 r 1 r $ strassenMixed a' b'
 | otherwise = joinBlocks (c11,c12,c21,c22)
 where
  n = quot r 2
  (a11,a12,a21,a22) = splitBlocks n n a
  (b11,b12,b21,b22) = splitBlocks n n b

  p1 = strassenMixed (a11 + a22) (b11 + b22)
  p2 = strassenMixed (a21 + a22)  b11
  p3 = strassenMixed  a11        (b12 - b22)
  p4 = strassenMixed        a22  (b21 - b11)
  p5 = strassenMixed (a11 + a12)        b22
  p6 = strassenMixed (a21 - a11) (b11 + b12)
  p7 = strassenMixed (a12 - a22) (b21 + b22)

  c11 = p1 + p4 - p5 + p7
  c12 = p3 + p5
  c21 = p2 + p4
  c22 = p1 - p2 + p3 + p6

-- | Функция умножения
multStrassenMixed :: Num a => Matrix a -> Matrix a -> Matrix a
multStrassenMixed a1@(M n m _) a2@(M n' m' _)
   | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and "
                    ++ sizeStr n' m' ++ " matrices."
   | n < strmixFactor = multStd_ a1 a2
   | otherwise =
       let mx = maximum [n,m,n',m']
           n2 = if even mx then mx else mx+1
           b1 = extendTo n2 n2 a1
           b2 = extendTo n2 n2 a2
       in  submatrix 1 n 1 m' $ strassenMixed b1 b2

-------------------------------------------------------
-------------------------------------------------------
---- Прдествитель для функтора

instance Functor Matrix where
 fmap f (M n m v) = M n m $ fmap (fmap f) v

-- | изменить строку функцией.
--   Пример:
--
-- >                          ( 1 2 3 )   ( 1 2 3 )
-- >                          ( 4 5 6 )   ( 5 6 7 )
-- > mapRow (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 8 9 )
--
mapRow :: (Int -> a -> a)
        -> Int           
        -> Matrix a -> Matrix a
mapRow f r (M n m v) =
    M n m $ V.imap (\i rx -> if i+1 == r then V.imap (f . succ) rx else rx) v

-------------------------------------------------------
-------------------------------------------------------
---- NUM Представитель

instance Num a => Num (Matrix a) where
 fromInteger = M 1 1 . V.singleton . V.singleton . fromInteger
 negate = fmap negate
 abs = fmap abs
 signum = fmap signum
 -- суммирование
 (M n m v) + (M n' m' v')
   | n /= n' || m /= m' = error $ "Addition of " ++ sizeStr n m ++ " and "
                               ++ sizeStr n' m' ++ " matrices."
   | otherwise = M n m $ V.zipWith (V.zipWith (+)) v v'
 (*) = multStrassenMixed

-------------------------------------------------------
-------------------------------------------------------
---- ПРЕОБРАЗОВАНИЯ 

-- | пОМНОЖИТЬ НА ЧИСЛО.
--   пример:
--
-- >               ( 1 2 3 )   (  2  4  6 )
-- >               ( 4 5 6 )   (  8 10 12 )
-- > scaleMatrix 2 ( 7 8 9 ) = ( 14 16 18 )
scaleMatrix :: Num a => a -> Matrix a -> Matrix a
scaleMatrix = fmap . (*)

-- | Домножить строку на число.
--   пример:
--
-- >              ( 1 2 3 )   (  1  2  3 )
-- >              ( 4 5 6 )   (  8 10 12 )
-- > scaleRow 2 2 ( 7 8 9 ) = (  7  8  9 )
scaleRow :: Num a => a -> Int -> Matrix a -> Matrix a
scaleRow = mapRow . const . (*)

-- |добавляет к строку другую строку помноженную на скаляр.
--   пример:
--
-- >                   ( 1 2 3 )   (  1  2  3 )
-- >                   ( 4 5 6 )   (  6  9 12 )
-- > combineRows 2 2 1 ( 7 8 9 ) = (  7  8  9 )
combineRows :: Num a => Int -> a -> Int -> Matrix a -> Matrix a
combineRows r1 l r2 m = mapRow (\j x -> x + l * getElem r2 j m) r1 m

-- | Поменять местами 2 строки.
--   пример:
--
-- >                ( 1 2 3 )   ( 4 5 6 )
-- >                ( 4 5 6 )   ( 1 2 3 )
-- > switchRows 1 2 ( 7 8 9 ) = ( 7 8 9 )
switchRows :: Int -- ^ строка 1.
           -> Int -- ^ строка 2.
           -> Matrix a -- ^ входная матрица.
           -> Matrix a 
switchRows r1 r2 (M n m vs) = M n m $ V.modify (\mv -> MV.swap mv (r1-1) (r2-1)) vs

-------------------------------------------------------
-------------------------------------------------------
---- СВОЙСTВА

-- | Сумма элементов главной диагонали.
--   Пример:
--
-- >       ( 1 2 3 )
-- >       ( 4 5 6 )
-- > trace ( 7 8 9 ) = 15
trace :: Num a => Matrix a -> a
trace = V.sum . getDiag

-- | произведение элементов главной диагонали (если определитель треугольной матрицы искать).
--   Пример:
--
-- >          ( 1 2 3 )
-- >          ( 4 5 6 )
-- > diagProd ( 7 8 9 ) = 45
diagProd :: Num a => Matrix a -> a
diagProd = V.product . getDiag

-- ДЕТЕРМИНАНТ

-- | Определитель по Лапласу.
detLaplace :: Num a => Matrix a -> a
detLaplace (M 1 1 v) = V.head (V.head v)
detLaplace m =
    sum [ (-1)^(i-1) * m ! (i,1) * detLaplace (minorMatrix i 1 m) | i <- [1 .. nrows m] ]


