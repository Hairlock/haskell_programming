module Garden where

    data FlowerType = Gardenia
                    | Daisy
                    | Rose
                    | Lilac
                    deriving Show

    -- type Gardener = String

    data Garden =
        Gardener Gardenia
        | Gardener Daisy
        | Gardener Rose
        | Gardener Lilac