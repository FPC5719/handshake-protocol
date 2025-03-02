module Protocol.Arbiter where

import Protocol.FSM
import Protocol.Boolish

import Clash.Prelude
import Control.Lens hiding (Index)
import Data.Maybe
import Data.Monoid

data Arbitrable req resp
  =  forall b rq rp
  .  ( Boolish b
     , Boolish rq
     , Boolish rp
     )
  => Arbitrable
     (Getter req  b )
     (Getter req  rq)
     (Getter resp rp)

type ArbiterRule n r
  =  (BitVector n, r)
  -> (Maybe (Index n), r)

arbiter
  :: ( NFDataX r, Monoid r
     , Monoid req, Monoid resp
     , KnownNat n
     )
  => ArbiterRule n r
  -> Arbitrable req resp
  -> FSM'
     (r, First (Index n))
     (Vec n req , resp)
     (Vec n resp, req , First (Index n))
arbiter rule (Arbitrable lb lrq lrp) =
  let grant idx (vec, resp) =
        ( mempty & ix idx .~ resp
        , vec ^. ix idx
        , pure idx
        )
      getBV vec = v2bv $
        vec & traverse %~ (boolToBit . boolify . view lb)
      finish idx (vec, resp) =
        boolify (vec ^. ix idx ^. lrq) &&
        boolify (resp ^. lrp)
  in FSM' $
     ( loop_ id $ embed_
       ( \(vec, resp) (r, _) -> case rule (getBV vec, r) of
             (Nothing , r') -> (mempty               , (r', mempty  ), False)
             (Just idx, r') -> (grant idx (vec, resp), (r', pure idx), True )
       )
     ) &>
     ( loop_ id $ embed_
       ( \(vec, resp) (r, idx') ->
           let idx = fromMaybe 0 . getFirst $ idx'
           in (grant idx (vec, resp), (r, idx'), finish idx (vec, resp))
       )
     )

ruleFP :: KnownNat n => ArbiterRule n ()
ruleFP (bv, ()) = (elemIndex high (bv2v bv), ())

ruleRR :: KnownNat n => ArbiterRule n (First (Index n))
ruleRR (bv :: BitVector n, idx') =
  let idx = fromMaybe 0 . getFirst $ idx'
      mask = rotateL (setBit (0 :: BitVector n) (fromIntegral idx)) 1 - 1
      upper = elemIndex high . bv2v $ bv .&. mask
      lower = elemIndex high . bv2v $ bv .&. complement mask
  in case upper <|> lower of
       Nothing -> (Nothing, idx')
       x -> (x, First x)
