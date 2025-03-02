module Tests.Simple where

import Protocol.FSM
import Protocol.Handshake

import Clash.Prelude
-- import Clash.Prelude.Testbench
import Control.Lens
import Data.Maybe
import Data.Monoid

-- import Test.Tasty
import Test.QuickCheck

lfsr :: Unsigned 4 -> Unsigned 4
lfsr x = unpack $ case pack x of
  $(bitPattern "abcd") -> b ++# c ++# d ++# (a `xor` b)
  other -> other

randBlock
  :: IsFSM r i o (Unsigned 4)
  => Unsigned 4
  -> Lens' r (First (Unsigned 4))
  -> FSM r i o (Unsigned 4) () ()
randBlock st l = FSM $ \_ r -> \case
  Left s -> if s == 0
    then Right ()
    else Left (mempty, r, s - 1)
  Right () ->
    let seed = fromMaybe st . getFirst $ r ^. l
    in Left (mempty, r & l .~ pure (lfsr seed), seed)

producer
  :: Unsigned 4
  -> FSM'
     (First (Unsigned 8), First (Unsigned 4))
     Ready
     (Channel (Unsigned 8))
producer st = FSM' $
  ( embed
    (\_ r () -> (mempty, (step (r ^. _1), r ^. _2)))
    (\_ _ -> ())
  ) &>
  send (Sender id id (_1 . to (fromMaybe 0 . getFirst))) &>
  randBlock st _2
  where
    step :: First (Unsigned 8) -> First (Unsigned 8)
    step = pure . maybe 0 (+ 1) . getFirst

consumer
  :: Unsigned 4
  -> FSM'
     ( First (Unsigned 8)
     , First (Unsigned 8)
     , First (Unsigned 4)
     )
     (Channel (Unsigned 8))
     (Ready, First Bool)
consumer st = FSM' $
  let myEq x y = case (==) <$> getFirst x <*> ((+ 1) <$> getFirst y) of
        Nothing -> pure True
        Just f -> pure f
  in listen (Listener _1 id _1) &>
     ( embed
       (\_ r () -> ((mempty, myEq (r ^. _1) (r ^. _2)), (mempty, r ^. _1, r ^. _3)))
       (\_ _ -> ())
     ) &>
     randBlock st _3
{-
prop_simple_producer :: Property
prop_simple_producer = testFor 12 (hideClockResetEnable circ')
  where
    circ' :: Clock System -> Reset System -> Enable System -> Signal System Bool
    circ' clk rst en = withClockResetEnable clk rst en circ
    circ :: HiddenClockResetEnable dom => Signal dom Bool
    circ =
      let d = mealyFSM' producer
            (stimuliGenerator
              $(listToVecTH
                 [ Ready False, Ready False, Ready False, Ready False
                 , Ready True, Ready True, Ready True, Ready True
                 , Ready True, Ready True, Ready False, Ready False
                 , Ready False, Ready False, Ready False, Ready False
                 ]))
      in (==) <$> d <*> stimuliGenerator
         $(listToVecTH
            [ Invalid, Valid 0, Valid 0, Valid 0
            , Invalid, Invalid, Valid 1, Invalid
            , Invalid, Valid 2, Valid 2, Valid 2
            , Valid 2, Valid 2, Valid 2, Valid 2 :: Channel (Unsigned 8)
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
-}
prop_simple :: Property
prop_simple = testFor 100 (hideClockResetEnable circ')
  where
    circ' :: Clock System -> Reset System -> Enable System -> Signal System Bool
    circ' clk rst en = withClockResetEnable clk rst en circ
    circ :: HiddenClockResetEnable dom => Signal dom Bool
    circ =
      let d = mealyFSM' (producer 3) r
          (r, f) = unbundle $ mealyFSM' (consumer 5) d
          check x = case getFirst x of
            Just False -> False
            _ -> True
      in check <$> f
