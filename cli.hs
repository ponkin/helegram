import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Put
import Data.Binary
import Data.Int
import Data.Time.Clock.POSIX
import System.Locale
import Data.Ratio
import System.Entropy

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

main :: IO ()
main = do
     time <- getPOSIXTime
     key <- getEntropy 16
     BL.putStr $ runPut $ wrap (numerator . toRational . (* 4096000000) $ time) (BL.toStrict . runPut $ req_pq key)