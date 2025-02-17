import Prelude

import Test.Tasty
import Test.Tasty.QuickCheck
import Tests.Simple

main :: IO ()
main = defaultMain $ testGroup "."
  [ testProperties "simple"
    [ ("simple_producer", prop_simple_producer)
    , ("simple_consumer", prop_simple_consumer)
    , ("simple", prop_simple)]
  ]
