data Item = Item
    { itemID :: String
    , nome :: String
    , quantidade :: Int
    , categoria :: String
    } deriving (Show, Read)
  
type Inventario = Data.Map.Map String Item

data AcaoLog
    = Add
    | Remove
    | Update
    | QueryFail
    deriving (Show, Read)
