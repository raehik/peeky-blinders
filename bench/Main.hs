import qualified Data.Persist as Persist
import qualified Data.Serialize as Cereal
import qualified Data.Store as Store
import Data.String.ToString
import qualified Data.Vector.Unboxed as Vu
import Gauge.Main
import qualified PeekyBlinders as Pb
import qualified Test.Tasty.HUnit as Tasty
import Prelude

main = do
  arrayBenchmarks <-
    initArrayGroup
      [ ( "peeky-blinders",
          let decoder = do
                size <- Pb.statically Pb.leSignedInt4
                Pb.statically $ Pb.staticArray @Vu.Vector Pb.leSignedInt4 $ fromIntegral size
           in either (const Nothing) Just . Pb.decodeByteString decoder
        ),
        ( "store",
          let decoder = do
                size <- Store.peek @Int32
                Vu.replicateM (fromIntegral size) $ Store.peek @Int32
           in either (const Nothing) Just . Store.decodeWith decoder
        ),
        ( "cereal",
          let decoder = do
                size <- Cereal.getInt32le
                Vu.replicateM (fromIntegral size) $ Cereal.getInt32le
           in either (const Nothing) Just . Cereal.runGet decoder
        )
      ]
  defaultMain
    [ bgroup "int32-le-triplet" $
        let input = Cereal.runPut (Cereal.putInt32le 1 <> Cereal.putInt32le 2 <> Cereal.putInt32le 3)
            b name peek = bench name $ nf peek input
         in [ b "peeky-blinders/statically" $ Pb.decodeByteString $ Pb.statically $ (,,) <$> Pb.leSignedInt4 <*> Pb.leSignedInt4 <*> Pb.leSignedInt4,
              b "peeky-blinders/dynamically" $ Pb.decodeByteString $ (,,) <$> Pb.statically Pb.leSignedInt4 <*> Pb.statically Pb.leSignedInt4 <*> Pb.statically Pb.leSignedInt4,
              b "store" $ either (const Nothing) Just . Store.decode @(Int32, Int32, Int32),
              b "cereal" $ Cereal.runGet ((,,) <$> Cereal.getInt32le <*> Cereal.getInt32le <*> Cereal.getInt32le),
              b "persist" $ Persist.runGet ((,,) <$> Persist.getLE @Int32 <*> Persist.getLE @Int32 <*> Persist.getLE @Int32)
            ],
      bgroup "array" arrayBenchmarks
    ]

-- | Test functions and create benchmarks out of them.
initArrayGroup :: [(String, ByteString -> Maybe (Vu.Vector Int32))] -> IO [Benchmark]
initArrayGroup = initGroup input correctDecoding
  where
    input =
      Cereal.runPut $
        Cereal.putInt32le 100 <> replicateM_ 100 (Cereal.putInt32le (-1))
    correctDecoding =
      Vu.replicate 100 (-1)

initGroup :: (Eq a, Show a, NFData a) => ByteString -> a -> [(String, ByteString -> Maybe a)] -> IO [Benchmark]
initGroup input correctDecoding subjects = do
  forM subjects $ \(name, f) -> do
    Tasty.assertEqual name (Just correctDecoding) (f input)
    return $ bench name $ nf f input
