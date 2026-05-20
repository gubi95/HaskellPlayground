module StorageCodec (StorageCodec(..)) where
    
class StorageCodec a where
    encode :: a -> String
    decode :: String -> Either String a