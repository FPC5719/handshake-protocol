module Tests.Simple where

import Protocol.FSM
import Protocol.Handshake

import Clash.Prelude
import Clash.Prelude.Testbench
import Control.Lens
import Data.Monoid

import Test.Tasty
import Test.QuickCheck


producer
  :: FSM'
     (First (Unsigned 8))
     Ready
     (Channel (Unsigned 8))
producer = FSM' $
  embedS (\_ -> (id %= step) >> pure mempty) &>
  send id id (id . to force)
  where
    step :: First (Unsigned 8) -> First (Unsigned 8)
    step = pure . maybe 0 (+ 1) . getFirst
    force :: First a -> a
    force (First (Just x)) = x

consumer
  :: FSM'
     ( First (Unsigned 8)
     , First (Unsigned 8)
     )
     (Channel (Unsigned 8))
     (Ready, First Bool)
consumer = FSM' $
  listen id _1 _1 &>
  ( embedS $ \_ -> do
      let myeq :: First (Unsigned 8) -> First (Unsigned 8) -> Bool
          myeq x y = case (==) <$> getFirst x <*> ((+ 1) <$> getFirst y) of
            Nothing -> True
            Just f -> f
      f <- myeq <$> use _1 <*> use _2
      use _1 >>= (_2 .=)
      pure $ (mempty, pure f)
  )

prop_simple_producer :: Property
prop_simple_producer = testFor 15 (hideClockResetEnable circ')
  where
    circ' :: Clock System -> Reset System -> Enable System -> Signal System Bool
    circ' clk rst en = withClockResetEnable clk rst en circ
    circ :: HiddenClockResetEnable dom => Signal dom Bool
    circ =
      let d = mealyFSM' producer
            (stimuliGenerator
              $(listToVecTH
                 [ Ready False, Ready False, Ready True, Ready True
                 , Ready True, Ready False, Ready False, Ready False
                 , Ready False, Ready True, Ready False, Ready False
                 , Ready False, Ready False, Ready False, Ready False
                 ]))
      in (==) <$> d <*> stimuliGenerator
         $(listToVecTH
            [ Invalid, Valid 0, Valid 0, Invalid
            , Valid 1, Invalid, Valid 2, Valid 2
            , Valid 2, Valid 2, Invalid, Valid 3
            , Valid 3, Valid 3, Valid 3, Valid 3 :: Channel (Unsigned 8)
            ])

prop_simple_consumer :: Property
prop_simple_consumer = testFor 15 (hideClockResetEnable circ')
  where
    circ' :: Clock System -> Reset System -> Enable System -> Signal System Bool
    circ' clk rst en = withClockResetEnable clk rst en circ
    circ :: HiddenClockResetEnable dom => Signal dom Bool
    circ =
      let (r, f) = unbundle $ mealyFSM' consumer
            (stimuliGenerator
              $(listToVecTH
                 [ Invalid, Valid 0, Invalid, Valid 1
                 , Invalid, Invalid, Valid 2, Invalid
                 , Invalid, Invalid, Invalid, Invalid
                 , Invalid, Invalid, Invalid, Invalid :: Channel (Unsigned 8)
                 ]))
      in (==) <$> r <*> stimuliGenerator
         $(listToVecTH
            [ Ready True, Ready True, Ready False, Ready True
            , Ready False, Ready True, Ready True, Ready False
            , Ready True, Ready True, Ready True, Ready True
            , Ready True, Ready True, Ready True, Ready True
            ])

prop_simple :: Property
prop_simple = testFor 100 (hideClockResetEnable circ')
  where
    circ' :: Clock System -> Reset System -> Enable System -> Signal System Bool
    circ' clk rst en = withClockResetEnable clk rst en circ
    circ :: HiddenClockResetEnable dom => Signal dom Bool
    circ =
      let d = mealyFSM' producer r
          (r, f) = unbundle $ mealyFSM' consumer d
          check x = case getFirst x of
            Just False -> False
            _ -> True
      in check <$> f
