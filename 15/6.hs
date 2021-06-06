-- 最初の近似値 1.0
-- 望みの差の範囲 0.00001

-- ニュートン法
sqroot :: Double -> Double
sqroot x = findSqroot withinDifference x (approximateValuesOf x)

-- 2つの差が望みの範囲(0.00001)内かどうか
withinDifference :: Double -> Double -> Bool
withinDifference former latter = (abs (former - latter)) <= 0.00001

-- 近似値のリスト
approximateValuesOf :: Double -> [Double]
approximateValuesOf x = iterate (\a -> (a + x/a) / 2.0) 1.0

-- 最終的に値を返す関数
findSqroot f i (x:xs) | f i x = x
                      | otherwise = findSqroot f x xs
