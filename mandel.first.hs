import List

-- Mini complex library (I want to understand the language, so no library-based complex)
data Complex = ComplexVal !Float !Float deriving (Show)
addC (ComplexVal a b) (ComplexVal c d) = ComplexVal (a+c) (b+d)
mulC (ComplexVal a b) (ComplexVal c d) = ComplexVal (a*c-b*d) (a*d+b*c)
lengthC (ComplexVal a b) = sqrt (a*a + b*b)

-- Mandelbrot inner loop: 
-- iteration number, accumulated Complex number, Complex number we are working on (pixel)
mandelImpl :: Int -> Complex -> Complex -> Int
mandelImpl iter ac pixel
    | iter > 120  = 0
    | otherwise   = let nVal = ac `mulC` ac `addC` pixel
                    in if lengthC nVal > 2.0
                       then iter
                       else mandelImpl (iter+1) nVal pixel

-- Bootstrap the calculation
mandel :: Complex -> Int
mandel = mandelImpl 1 (ComplexVal 0.0 0.0)

-- Do the mandelbrot calculations for a scanline of pixels
-- number of pixels, starting (leftside) complex number, Complex step to move to the right per pixel
scanline :: Int -> Complex -> Complex -> [Int]
scanline pixels start horizStep  
    | pixels == 0      = []
    | otherwise        = mandel start : scanline (pixels-1) (start `addC` horizStep) horizStep
-- shorter version, more difficult to grasp, slower (multiples with step), with list comprehension:
--scanline pixels start step = [mandel (start `addC` (ComplexVal (fromIntegral i) 0.0 `mulC` step)) | i<-[0..(pixels-1)]]

-- Create (pixelsY scanlines) of (pixelX integers) each
-- number of scanlines, number of pixels per scanline, bottom-left complex to start from, vertical step per scanline
scanlines :: Int -> Int -> Complex -> Complex -> [[Int]]
scanlines pixelsX pixelsY start stepVertical
    | pixelsY == 0     = []
    | otherwise        = scanline pixelsX start (ComplexVal (3.2 / fromIntegral pixelsX ) 0.0)
                         : scanlines pixelsX (pixelsY-1) (start `addC` stepVertical) stepVertical

-- Render the scanlines [[Int]] into PNM data ("1 1 3 4\n1 0 3 1\n...")
emitPNMdata :: [[Int]] -> String
emitPNMdata (xs:xss) = concat (List.intersperse " " [show x| x<-xs]) ++ "\n" ++ emitPNMdata xss
emitPNMdata [] = ""

-- Prepend the PNM header before 
emitPNM :: [[Int]] -> String
emitPNM xss = "P2\n" ++ show (length (head xss)) ++ "\n" ++ show (length xss) ++ "\n255\n" ++ emitPNMdata (reverse xss)

bottomLeft   = ComplexVal (0.0-2.0) (0.0-1.2)
verticalStep = ComplexVal 0.0 (2.4/600)
mandelData   = scanlines 800 600 bottomLeft verticalStep

main = putStrLn $ emitPNM mandelData
