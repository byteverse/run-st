{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language UnboxedTuples #-}

module Control.Monad.ST.Run
  ( runArrayST
  , runSmallArrayST
  , runByteArrayST
  , runPrimArrayST
  , runUnliftedArrayST
  , runIntST
  , runWordST
  ) where

import GHC.Exts (Int(I#),Word(W#),runRW#)
import Data.Primitive.Array (Array(Array))
import Data.Primitive.SmallArray (SmallArray(SmallArray))
import Data.Primitive.ByteArray (ByteArray(ByteArray))
import Data.Primitive.PrimArray (PrimArray(PrimArray))
import Data.Primitive.Unlifted.Array (UnliftedArray(UnliftedArray))
import GHC.ST (ST(ST))

runArrayST :: (forall s. ST s (Array a)) -> Array a
{-# inline runArrayST #-}
runArrayST f = Array (runRW# (\s0 -> case f of { ST g -> case g s0 of { (# _, Array r #) -> r }}))

runSmallArrayST :: (forall s. ST s (SmallArray a)) -> SmallArray a
{-# inline runSmallArrayST #-}
runSmallArrayST f = SmallArray (runRW# (\s0 -> case f of { ST g -> case g s0 of { (# _, SmallArray r #) -> r }}))

runByteArrayST :: (forall s. ST s ByteArray) -> ByteArray
{-# inline runByteArrayST #-}
runByteArrayST f = ByteArray (runRW# (\s0 -> case f of { ST g -> case g s0 of { (# _, ByteArray r #) -> r }}))

runPrimArrayST :: (forall s. ST s (PrimArray a)) -> PrimArray a
{-# inline runPrimArrayST #-}
runPrimArrayST f = PrimArray (runRW# (\s0 -> case f of { ST g -> case g s0 of { (# _, PrimArray r #) -> r }}))

runUnliftedArrayST :: (forall s. ST s (UnliftedArray a)) -> UnliftedArray a
{-# inline runUnliftedArrayST #-}
runUnliftedArrayST f = UnliftedArray (runRW# (\s0 -> case f of { ST g -> case g s0 of { (# _, UnliftedArray r #) -> r }}))

runIntST :: (forall s. ST s Int) -> Int
{-# inline runIntST #-}
runIntST f = I# (runRW# (\s0 -> case f of { ST g -> case g s0 of { (# _, I# r #) -> r }}))

runWordST :: (forall s. ST s Word) -> Word
{-# inline runWordST #-}
runWordST f = W# (runRW# (\s0 -> case f of { ST g -> case g s0 of { (# _, W# r #) -> r }}))
