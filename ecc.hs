import Control.Monad

type ECConf = (Int, Int, Int)
data EC a = EC (ECConf -> a)

instance Functor EC where
    fmap f (EC e) = EC (\c -> f (e c) )

instance Applicative EC where
    pure k = EC (\c -> k)
    (EC f) <*> (EC e) = EC (\c -> (f c) (e c) )

instance Monad EC where
    (EC e) >>= f = EC (\c -> let EC e' = f (e c) in e' c)

runEC :: EC a -> ECConf -> a
runEC (EC e) c = e c

-------------------------------

data CurvePoint = Point Int Int | Infinity deriving Eq

instance Show CurvePoint where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"
    show Infinity = "O"

findPoints :: Int -> Int -> Int -> [CurvePoint]
findPoints p a b = let 
    xs = [0..(p-1)]
    fx = (\x -> (x^3 + a*x + b) `mod` p)
    fy = (\y -> y^2 `mod` p)
    in [ Point x y | x<-xs, y<-xs, fy y == fx x ]

allPoints :: EC [CurvePoint]
allPoints = EC (\(p,a,b) -> (Infinity : (findPoints p a b)))

-----------------------------

toRep :: Int -> EC Int
toRep i = EC (\(p,_,_) -> i `mod` p)

invertPoint :: CurvePoint -> EC CurvePoint
invertPoint (Point x y) = fmap (Point x) (toRep (-y))

divide :: Int -> Int -> EC Int
divide n d = EC (\(p,_,_) -> (n * (d^(p-2))) `mod` p )

slope :: Int -> (Int,Int) -> (Int,Int)-> EC Int
slope a (x1,y1) (x2,y2)
    | x1 == x2 = divide ((3 * (x1^2)) + a) (2 * y1)
    | otherwise = divide (y1 - y2) (x1 - x2)

curveOp :: CurvePoint -> CurvePoint -> EC CurvePoint
curveOp (Point x1 y1) (Point x2 y2) = do
    a <- EC (\(_,a,_) -> a)
    m <- slope a (x1,y1) (x2,y2)
    x <- toRep (m^2 - x1 - x2)
    y <- toRep (m * (x - x1) + y1)
    invertPoint (Point x y)

addPoints :: CurvePoint -> CurvePoint -> EC CurvePoint
addPoints Infinity p = return p
addPoints p Infinity = return p
addPoints p q = (invertPoint q) >>= (\nq -> if p == nq then return Infinity else curveOp p q)

collect :: (a -> EC b) -> [a] -> EC [b]
collect f inp = foldM (\l x -> (f x) >>= (\fx -> return (l ++ [fx]) )) [] inp

order :: CurvePoint -> Int -> CurvePoint -> EC Int
order _ n Infinity = return n
order p n t = (addPoints p t) >>= (order p (n+1))

allOrders :: EC [Int]
allOrders = allPoints >>= (collect (\p -> order p 1 p))

subgroup :: CurvePoint -> CurvePoint -> EC [CurvePoint]
subgroup _ Infinity = return [Infinity]
subgroup p np = do
    q <- addPoints np p
    sg <- subgroup p q
    return (np : sg)

allSubgroups :: EC [[CurvePoint]]
allSubgroups = allPoints >>= (collect (\p -> subgroup p p))

-----------------------------

r1 ec = runEC ec (7,2,6)
r2 ec = runEC ec (13,0,1)
r3 ec = runEC ec (7,3,2)
r4 ec = runEC ec (7,1,1)
