import Clash.Prelude
import Clash.Prelude.Testbench

import Protocol.FSM
import Protocol.Handshake

import Tests.Simple

import Data.Monoid

main :: IO ()
main = do
  let circ' :: Clock System -> Reset System -> Enable System -> Signal System (Channel (Unsigned 8), Ready)
      circ' clk rst en = withClockResetEnable clk rst en circ
      circ :: HiddenClockResetEnable dom => Signal dom (Channel (Unsigned 8), Ready)
      circ =
        let d = mealyFSM' producer r
            (r, f) = unbundle $ mealyFSM' consumer d
        in bundle (d, r)
  print $ sampleN @System 20 (hideClockResetEnable circ')
