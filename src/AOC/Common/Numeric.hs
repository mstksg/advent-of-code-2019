{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE TypeInType                               #-}
{-# LANGUAGE TypeOperators                            #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module AOC.Common.Numeric (
    fft
  , ifft
  , convolve
  , rconvolve
  , zconvolve
  , FFT.FFTWReal
  ) where

import           Data.Complex
import           GHC.TypeNats
import qualified Data.Array.CArray         as CA
import qualified Data.Array.IArray         as IA
import qualified Data.Ix                   as Ix
import qualified Data.Vector.Generic       as VG
import qualified Data.Vector.Generic.Sized as SVG
import qualified Foreign.Storable          as FS
import qualified Math.FFT                  as FFT
import qualified Math.FFT.Base             as FFT

fft :: (FFT.FFTWReal a, VG.Vector v (Complex a))
    => SVG.Vector v n (Complex a)
    -> SVG.Vector v n (Complex a)
fft = SVG.withVectorUnsafe $
        fromCA
      . FFT.dft
      . toCA

ifft
    :: (FFT.FFTWReal a, VG.Vector v (Complex a))
    => SVG.Vector v n (Complex a)
    -> SVG.Vector v n (Complex a)
ifft = SVG.withVectorUnsafe $
        fromCA
      . FFT.idft
      . toCA

fromCA
    :: (FS.Storable a, VG.Vector v (Complex a))
    => CA.CArray Int (Complex a)
    -> v (Complex a)
fromCA v = VG.generate (Ix.rangeSize (IA.bounds v)) (v IA.!)

toCA
    :: (FS.Storable a, VG.Vector v (Complex a))
    => v (Complex a)
    -> CA.CArray Int (Complex a)
toCA v = IA.listArray (0, VG.length v - 1) (VG.toList v)

-- | FFT-based convolution
convolve
    :: ( VG.Vector v (Complex a)
       , KnownNat n, 1 <= n
       , KnownNat m, 1 <= m
       , FFT.FFTWReal a
       )
    => SVG.Vector v n (Complex a)
    -> SVG.Vector v m (Complex a)
    -> SVG.Vector v (n + m - 1) (Complex a)
convolve x y = ifft $ fft x' * fft y'
  where
    x' = x SVG.++ 0
    y' = y SVG.++ 0

-- | FFT-based real-valued convolution
rconvolve
    :: ( VG.Vector v (Complex a)
       , VG.Vector v a
       , KnownNat n, 1 <= n
       , KnownNat m, 1 <= m
       , FFT.FFTWReal a
       )
    => SVG.Vector v n a
    -> SVG.Vector v m a
    -> SVG.Vector v (n + m - 1) a
rconvolve x y = SVG.map realPart $ convolve (SVG.map (:+ 0) x) (SVG.map (:+ 0) y)

-- | FFT-based integral convolution
zconvolve
    :: ( VG.Vector v (Complex Double)
       , VG.Vector v Double
       , VG.Vector v a
       , KnownNat n, 1 <= n
       , KnownNat m, 1 <= m
       , Integral a
       )
    => SVG.Vector v n a
    -> SVG.Vector v m a
    -> SVG.Vector v (n + m - 1) a
zconvolve x y = SVG.map (round @Double) $
    rconvolve (SVG.map fromIntegral x) (SVG.map fromIntegral y)
