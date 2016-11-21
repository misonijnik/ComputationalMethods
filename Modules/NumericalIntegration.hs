module NumericalIntegration where

import FuncToTableHelper

data IntegrationType = MediumRectangle | Trapeze | Simpson
    deriving Eq

function :: ValueFunc
function x = 2*x + exp x
--function x = x**2

pervFunction :: ValueFunc
pervFunction x = x ** 2 + exp x
--pervFunction x = x ** 3 / 3

functionIntegral :: Segment -> Value
functionIntegral (a, b) = pervFunction b - pervFunction a

mediumRectangle :: ValueFunc -> Value -> Value -> Value
mediumRectangle f a b = (b - a) * f ((a + b) / 2)

trapeze :: ValueFunc -> Value -> Value -> Value
trapeze f a b = c * (f a + f b)
    where c = (b - a) / 2 

simpson :: ValueFunc -> Value -> Value -> Value
simpson f a b = (*) c $ f a + 4 * f ((a + b )/ 2) + f b
    where c = (b - a) / 6

convolution :: (Value -> Value -> Value) -> [Value] -> Value
convolution _ [] = 0
convolution _ [_] = 0
convolution f [x, y] = f x y
convolution f (x : y : list) = f x y + convolution f (y : list)

integration :: ValueFunc -> Segment -> Int -> IntegrationType-> Value
integration f seg m t | t == MediumRectangle = res mediumRectangle
                      | t == Trapeze         = res trapeze
                      | t == Simpson         = res simpson
                      | otherwise            = error "Emm..."
    where values     = divisionIntoUnits m seg
          res method = convolution (method f) values 
