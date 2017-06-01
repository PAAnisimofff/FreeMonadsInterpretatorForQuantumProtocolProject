module Common.Complex(Complex((:+)), realPart, imagPart)  where

infix  6  :+

data Complex a = !a :+ !a  deriving (Eq,Read,Show)


realPart, imagPart :: (RealFloat a) => Complex a -> a
realPart (x:+y)  =  x
imagPart (x:+y)  =  y

magnitude :: (RealFloat a) => Complex a -> a
magnitude (x:+y) =  scaleFloat k
     (sqrt ((scaleFloat mk x)^2 + (scaleFloat mk y)^2))
    where k  = max (exponent x) (exponent y)
          mk = - k


instance  (RealFloat a) => Num (Complex a)  where
    (x:+y) + (x':+y') =  (x+x') :+ (y+y')
    (x:+y) - (x':+y') =  (x-x') :+ (y-y')
    (x:+y) * (x':+y') =  (x*x'-y*y') :+ (x*y'+y*x')
    negate (x:+y) =  negate x :+ negate y
    abs z =  magnitude z :+ 0
    signum 0 =  0
    signum z@(x:+y) =  x/r :+ y/r  where r = magnitude z
    fromInteger n =  fromInteger n :+ 0
