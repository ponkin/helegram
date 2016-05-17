import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary
import Data.Int
import Data.Time.Clock.POSIX
import System.Locale
import Data.Ratio
import System.Entropy
import Data.Digest.CRC32
import Network.Simple.TCP hiding (recv)
import Network.Socket.ByteString.Lazy

data TcpFullPack = TcpFullPack
  { len :: !Word32
  , seqNum :: !Word32
  , body   :: ResPQ
  , crc       :: !Int32
  } deriving (Show)

data ResPQ = ResPQ
  { auth_key_id :: !Word64
  , message_id :: !Word64
  , message_length :: !Word32
  , constructor :: !Word32
  , nonce :: B.ByteString
  , server_nonce :: B.ByteString
  , pq :: B.ByteString
  , vector_long :: !Word32
  , count :: !Word32  
  , fingerprints :: B.ByteString 
  } deriving (Show)


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

unwrap :: Get TcpFullPack
unwrap = do
    len <- getWord32le
    seqNum <- getWord32le
    body <- getLazyByteString $ fromIntegral $ len - 12
    crc <- getInt32le
    return $! TcpFullPack len seqNum (runGet unwrapResPq body) crc

unwrapResPq :: Get ResPQ
unwrapResPq = do
    auth_key_id <- getWord64le
    message_id <- getWord64le
    message_length <- getWord32le
    constructor <- getWord32le
    nonce <- getByteString $ fromIntegral $ 16
    server_nonce <- getByteString $ fromIntegral $ 16
    pq <- getByteString $ fromIntegral $ 12
    vector_long <- getWord32le
    count <- getWord32le
    fingerprints <- getByteString $ fromIntegral $ 8
    return $! ResPQ auth_key_id message_id message_length constructor nonce server_nonce pq vector_long count fingerprints

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
-- main = do
--     time <- getPOSIXTime
--     key <- getEntropy 16
--     BL.putStr $ runPut $ tcp_pack_full 0 $ (BL.toStrict . runPut $ wrap (numerator . toRational . (* 4096000000) $ time) (BL.toStrict . runPut $ req_pq key))
main = do 
    connect "149.154.167.40" "443" $ \(connectionSocket, remoteAddr) -> do
        time <- getPOSIXTime
        key <- getEntropy 16
        let req = runPut $ tcp_pack_full 0 $ (BL.toStrict . runPut $ wrap (numerator . toRational . (* 4096000000) $ time) (BL.toStrict . runPut $ req_pq key))
        sendLazy connectionSocket req
        resp <- recv connectionSocket 100000
        putStrLn $ show $ runGet unwrap resp
