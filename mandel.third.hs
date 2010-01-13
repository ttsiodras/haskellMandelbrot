import List
import Complex

-- Mandelbrot inner loop: 
-- iteration number, accumulated Complex so far, Complex we are working on (pixel coordinates)
mandelImpl :: Int -> Complex Float -> Complex Float -> Int
mandelImpl iter ac c
    | iter > 120  = 0
    | otherwise   = let ac' = ac*ac + c -- C_{n+1} = C_n * C_n + C_pixel
                    in if magnitude ac' > 2.0
                       then iter
                       else mandelImpl (iter+1) ac' c

-- Bootstrap the calculation
mandel :: Complex Float -> Int
mandel = mandelImpl 1 (0.0:+0.0)

-- Render the scanlines [[Int]] into PNM data ("1 1 3 4\n1 0 3 1\n...")
emitPNMdata :: [[Int]] -> String
emitPNMdata (xs:xss) = concat (List.intersperse " " [show x| x<-xs]) ++ "\n" ++ emitPNMdata xss
emitPNMdata [] = ""

-- Prepend the PNM header before, and reverse the scanlines (since we start from the bottom)
emitPNM :: [[Int]] -> String
emitPNM xss = "P2\n" ++ show (length (head xss)) ++ "\n" ++ show (length xss) ++ "\n255\n" ++ emitPNMdata (reverse xss)

horizPixels :: Int = 800
vertPixels :: Int  = 600
bottomLeft   = (-2.0) :+ (-1.2)
upRight      = 1.2  :+ 1.2
horizStep    = (realPart (upRight - bottomLeft) / fromIntegral horizPixels) :+ 0.0
vertStep     = 0.0 :+ (imagPart (upRight - bottomLeft) / fromIntegral vertPixels)

(*^) :: Complex Float -> Int -> Complex Float
c *^ i = c * (fromIntegral i :+ 0.0)

mandelData   = [[mandel (bottomLeft + (vertStep *^ y) + (horizStep *^ x))
		    | x<-[1..horizPixels]] | y<-[1..vertPixels]]

main = putStrLn (emitPNM mandelData)
