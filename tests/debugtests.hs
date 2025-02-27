import Clash.Prelude
-- import Clash.Prelude.Testbench

import Protocol.FSM
import Protocol.Handshake

import Tests.Simple

import qualified Data.List as L
-- import Data.Monoid

type Output = (Ready, Channel (Unsigned 8))

main :: IO ()
main = do
  let circ' :: Clock System -> Reset System -> Enable System -> Signal System Output
      circ' clk rst en = withClockResetEnable clk rst en circ
      circ :: HiddenClockResetEnable dom => Signal dom Output
      circ =
        let d = mealyFSM' producer r
            (r, _) = unbundle $ mealyFSM' consumer d
        in bundle (r, d)
  pretty $ sampleN @System 100 (hideClockResetEnable circ')

pretty :: Show a => [a] -> IO ()
pretty
  = sequence_
  . L.map (\(i, a) -> putStrLn $ "Cycle " L.++ show i L.++ ": " L.++ show a)
  . L.zip [1 :: Int .. ]
