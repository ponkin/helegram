import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Put
import Data.Binary
import Data.Int
import Data.Time.Clock.POSIX
import System.Locale
import Data.Ratio
import System.Entropy
import Data.Digest.CRC32

req_pq :: B.ByteString -> Put
req_pq nonce = do
        putInt32le 1615239032
        putByteString $ nonce

wrap :: Integer -> B.ByteString -> Put
wrap exactUnixTime bytes = do
      putInt64le 0
      putInt64le $ fromIntegral $ exactUnixTime
      putInt32le $ fromIntegral $ B.length bytes
      putByteString $ bytes

calcCrc32 :: B.ByteString -> Put
calcCrc32 bytes = do
           putByteString $ bytes
           putWord32le $ crc32 bytes

tcp_pack_full :: Integer -> B.ByteString -> Put
tcp_pack_full seqNum bytes = calcCrc32 $ BL.toStrict . runPut $ do
      putInt32le $ fromIntegral $ 12 + B.length bytes
      putInt32le $ fromIntegral $ seqNum
      putByteString $ bytes

main :: IO ()
main = do
     time <- getPOSIXTime
     key <- getEntropy 16
     BL.putStr $ runPut $ tcp_pack_full 0 $ (BL.toStrict . runPut $ wrap (numerator . toRational . (* 4096000000) $ time) (BL.toStrict . runPut $ req_pq key))