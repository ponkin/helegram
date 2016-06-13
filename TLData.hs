module TLData (
 readString,
 getVarInteger,
 getVectorLong
) where

import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Binary
import Data.ByteString.Char8 as C
import Data.Bits


getWord24le :: Get Int
getWord24le = do
    a <- getWord8
    b <- getWord8
    c <- getWord8
    let low = fromIntegral(a)
        mid = fromIntegral(b) `shiftL` 8
        hi  = fromIntegral(c) `shiftL` 16
    return $ low .|. mid .|. hi

getVarByteString :: Int -> Get ByteString
getVarByteString n
        | n >= 254 = do
                     new_len <- getWord24le
                     actual <- getByteString new_len
                     let rem = mod (new_len + 4) 4
                     skip $ if rem > 0 then (4 - rem) else 0
                     return $ actual
        | otherwise = do
                      actual <- getByteString n
                      let rem = mod (n + 1) 4
                      skip $ if rem > 0 then (4 - rem) else 0
                      return $ actual

readString :: Get String
readString = do
    len <- getWord8
    fmap C.unpack $ getVarByteString $ (fromIntegral len)
    
shiftVarInt :: (Int, Integer) -> Word8 -> (Int, Integer)
shiftVarInt (shift, acc) b = (shift - 1, (fromIntegral(b) `shiftL` (8 * shift)) .|. acc )
    
getVarInteger :: Get Integer
getVarInteger = do
    len <- getWord8
    raw_int <- getVarByteString $ fromIntegral len
    let total_len = C.length raw_int
    return $ snd $ B.foldl shiftVarInt (total_len - 1, 0) raw_int
    
vector_long_step :: Int -> Get [Integer]
vector_long_step step
                  | step == 0 = do
                                return $ []
                  | otherwise = do
                                item <- getWord64le
                                rest <- vector_long_step $ step-1
                                return (fromIntegral(item) : rest)
    
getVectorLong :: Get [Integer]
getVectorLong = do
    constructor <- getWord32le
    num <- getWord32le
    vector_long_step $ fromIntegral num