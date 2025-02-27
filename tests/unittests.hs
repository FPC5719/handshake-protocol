import Prelude

import Test.Tasty
import Test.Tasty.QuickCheck
import Tests.Simple

main :: IO ()
main = defaultMain $ testGroup "."
  [ testProperties "simple"
    [ ("simple", prop_simple)
    ]
  ]
