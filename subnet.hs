import Data.List
import Data.List.Split
import Data.Char
import System.Environment

main = do
    args <- getArgs
    if not (validIp (args !! 0)) || not (validSlash (args !! 1)) 
    then do
        putStrLn "Invalid IP or slash!"
    else do
        putStrLn ("IP: " ++ (args !! 0) ++ " " ++ (args !! 1))
        putStrLn ("Netmask: " ++
            (slashIntToMask (slashStringToInt (args !! 1))))
        putStrLn ("Number of host addresses: " ++ 
            (show (numOfHostAddresses (args !! 1))))
        putStrLn ("Network address: " ++
            (networkAddress (args !! 0) (args !! 1)))
        putStrLn ("First host address: " ++ 
            (firstHostAddress (args !! 0) (args !! 1)))
        putStrLn ("Last host address: " ++ 
            (lastHostAddress (args !! 0) (args !! 1)))
        putStrLn ("Broadcast address: " ++ 
            (broadcastAddress (args !! 0) (args !! 1)))


binToDec :: String -> Int
binToDec xs = sum (zipWith (*) (reverse intxs) [ 2^n | n <- [0..] ])
    where intxs = map digitToInt xs

decToBin :: Int -> String
decToBin 0 = ['0']
decToBin x = reverse (decToBinHelper x)

decToBinHelper :: Int -> String
decToBinHelper 0 = []
decToBinHelper x 
    | x `mod` 2 == 0 = '0' : (decToBinHelper (x `div` 2))
    | x `mod` 2 == 1 = '1' : (decToBinHelper (x `div` 2))

stringToInt :: String -> Int
stringToInt = (\x -> read x :: Int)



rpadBinToEight :: String -> String
rpadBinToEight binary = binary ++ (replicate n '0')
    where n = 8 - length binary

lpadBinToEight :: String -> String
lpadBinToEight binary = (replicate n '0') ++ binary
    where n = 8 - length binary

ipDecToInts :: String -> [Int]
ipDecToInts ip = map stringToInt (splitOn "." ip)

ipDecToBinList :: String -> [String]
ipDecToBinList ip = map (lpadBinToEight . decToBin) (ipDecToInts ip)

ipDecToBin :: String -> String
ipDecToBin ip = intercalate "." (ipDecToBinList ip)


ipBinListToDec :: [String] -> String
ipBinListToDec bins = intercalate "." (map (show . binToDec) bins)



slashStringToInt :: String -> Int
slashStringToInt ('/':cs) = read cs :: Int

slashIntToInts :: Int -> [Int]
slashIntToInts 32 = take 4 (repeat 255)
slashIntToInts n =
    ((take q (repeat 255)) ++ 
    [binToDec (rpadBinToEight (replicate r '1'))] ++ 
    (take (3-q) (repeat 0)))
    where   r = n `mod` 8
            q = n `div` 8

slashIntToMask :: Int -> String
slashIntToMask n = intercalate "." (map show (slashIntToInts n))



allZerosRight :: Int -> String -> String
allZerosRight n bin = (take n bin) ++ (take (8-n) (repeat '0'))

allOnesRight :: Int -> String -> String
allOnesRight n bin = (take n bin) ++ (take (8-n) (repeat '1'))

networkAddress :: String -> String -> String
networkAddress ip "/32" = ip
networkAddress ip slash = ipBinListToDec 
    (xs ++ 
    [(allZerosRight r (head ys))] ++ 
    (take (3-q) (repeat "00000000")))
    where 
        n = slashStringToInt slash
        q = n `div` 8
        r = n `mod` 8
        (xs, ys) = splitAt q (ipDecToBinList ip)

broadcastAddress :: String -> String -> String
broadcastAddress ip "/32" = ip
broadcastAddress ip slash = ipBinListToDec 
    (xs ++ 
    [(allOnesRight r (head ys))] ++ 
    (take (3-q) (repeat "11111111")))
    where 
        n = slashStringToInt slash
        q = n `div` 8
        r = n `mod` 8
        (xs, ys) = splitAt q (ipDecToBinList ip)

firstHostAddress :: String -> String -> String
firstHostAddress _ "/31" = "no host addresses"
firstHostAddress _ "/32" = "no host addresses"
firstHostAddress ip slash = intercalate "." 
    ((map show xs) ++ [(show . (+1)) (head y)])
    where
        (xs, y) = splitAt 3 (ipDecToInts (networkAddress ip slash))

lastHostAddress :: String -> String -> String
lastHostAddress _ "/31" = "no host addresses"
lastHostAddress _ "/32" = "no host addresses"
lastHostAddress ip slash = intercalate "." 
    ((map show xs) ++ [(show . (+(-1))) (head y)])
    where
        (xs, y) = splitAt 3 (ipDecToInts (broadcastAddress ip slash))

numOfHostAddresses :: String -> Int
numOfHostAddresses "/32" = 0
numOfHostAddresses slash = 2^(32-s)-2
    where
        s = slashStringToInt slash



validIp :: String -> Bool
validIp ip = (4 == sum [ 1 | x <- xs, (0 <= x), (x <= 255) ])
    where 
        xs = ipDecToInts ip

validSlash :: String -> Bool
validSlash slash = (0 <= n) && (n <= 32)
    where
        n = slashStringToInt slash
