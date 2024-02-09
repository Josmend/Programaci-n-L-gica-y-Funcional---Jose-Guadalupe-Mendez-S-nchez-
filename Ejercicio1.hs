applyDiscount :: Float -> Float -> Float
applyDiscount price discount = price - price * discount / 100

applyIVA :: Float -> Float -> Float
applyIVA price percentage = price + price * percentage / 100

priceCesta :: [(Float, Float)] -> (Float -> Float -> Float) -> Float
priceCesta cesta function = sum [function price discount | (price, discount) <- cesta]

main :: IO ()
main = do
    putStrLn $ "El precio de la compra tras aplicar los descuentos es: " ++ show (priceCesta [(1000, 20), (500, 10), (100, 1)] applyDiscount)
    putStrLn $ "El precio de la compra tras aplicar el IVA es: " ++ show (priceCesta [(1000, 20), (500, 10), (100, 1)] applyIVA)
