import Clash.Prelude
-- import Clash.Prelude.Testbench

import Protocol.FSM
import Protocol.Handshake
import Protocol.Arbiter

import Control.Lens hiding ((:>), Index)
import qualified Data.List as L
import Data.Maybe
import Data.Monoid

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
  randBlock st _2 &>
  ( embed
    (\_ r () -> (mempty, (step (r ^. _1), r ^. _2)))
    (\_ _ -> ())
  ) &>
  send (Sender id id (_1 . to (fromMaybe 0 . getFirst)))
  where
    step :: First (Unsigned 8) -> First (Unsigned 8)
    step = pure . maybe 0 (+ 1) . getFirst

producerFake
  :: Unsigned 4
  -> FSM'
     (First (Unsigned 8), First (Unsigned 4))
     Ready
     (Channel (Unsigned 8))
producerFake st = FSM' $
  ( embed
    (\_ r () -> (mempty, (step (r ^. _1), r ^. _2)))
    (\_ _ -> ())
  ) &>
  -- send (Sender id id (_1 . to (fromMaybe 0 . getFirst))) &>
  randBlock st _2
  where
    step :: First (Unsigned 8) -> First (Unsigned 8)
    step = pure . maybe 0 (+ 1) . getFirst

consumer
  :: Unsigned 4
  -> FSM'
     ( First (Unsigned 8)
     , First (Unsigned 4)
     )
     (Channel (Unsigned 8))
     Ready
consumer st = FSM' $
  listen (Listener id id _1) &>
  ( embed
    (\_ r () -> (mempty, (mempty, r ^. _2)))
    (\_ _ -> ())
  ) &>
  randBlock st _2

blackHole
  :: FSM' () (Channel (Unsigned 8)) Ready
blackHole = FSM' $
  ( embed
    (\_ _ _ -> (Ready True, ()))
    (\_ _ -> ()) 
  )

arb :: Arbitrable (Channel (Unsigned 8)) Ready
arb = Arbitrable
  id (to (const True)) id

type Output =
  ( Channel (Unsigned 8)
  , Channel (Unsigned 8)
  , Ready
  , Ready
  , Ready
  , First (Index 2)
  )

main :: IO ()
main = do
  let circ' :: Clock System -> Reset System -> Enable System -> Signal System Output
      circ' clk rst en = withClockResetEnable clk rst en circ
      circ :: HiddenClockResetEnable dom => Signal dom Output
      circ =
        let ch1 = mealyFSM' (producer 0) r1
            ch2 = mealyFSM' (producer 1) r2
            vch = bundle $ ch1 :> ch2 :> Nil
            r1 :> r2 :> Nil = unbundle vr
            (vr, ch, idx) = unbundle . mealyFSM' (arbiter ruleFP arb) . bundle $ (vch, r)
            -- r = mealyFSM' (consumer 2) ch
            r = mealyFSM' (blackHole) ch
        in bundle (ch1, ch2, r, r1, r2, idx)
  pretty $ sampleN @System 100 (hideClockResetEnable circ')

pretty :: Show a => [a] -> IO ()
pretty
  = sequence_
  . L.map (\(i, a) -> putStrLn $ "Cycle " L.++ show i L.++ ": " L.++ show a)
  . L.zip [1 :: Int .. ]
