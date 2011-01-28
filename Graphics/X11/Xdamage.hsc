--------------------------------------------------------------------
-- |
-- Module    : Graphics.X11.Xdamage
-- Copyright : (c) Haskell.org, 2008
-- License   : BSD3
--
-- Maintainer: Ewan Higgs <ewan_higgs@yahoo.co.uk>
-- Stability : unstable 
-- Portability: unportable 
--
--------------------------------------------------------------------
--
-- Interface to Xdamage API
--

module Graphics.X11.Xdamage(
    DamageReportLevel,
    DamageNotify(..),
    Damage,
    xdamageAdd,
    xdamageCreate,
    xdamageDestroy,
    xdamageSubtract,
    xdamageQueryExtension,
    xdamageQueryVersion
 ) where

import Foreign
import Foreign.C.Types
import Graphics.X11.Xlib

-- | DAMAGE is a 32 bit value where the top three bits are guaranteed to be 0
type Damage = CInt

-- | 
type DamageReportLevel = CInt

{--
                         DamageReportRawRectangles
                       | DamageReportDeltaRectangles
                       | DamageReportBoundingBox
                       | DamageReportNonEmpty
--}
data DamageNotify  = DamageNotify
                   { xdn_type       :: CInt,
                     xdn_serial     :: CUInt,
                     xdn_send_event :: Bool,
                     xdn_display    :: Display,
                     xdn_drawable   :: Drawable,
                     xdn_damage     :: Damage,
                     xdn_level      :: CInt,
                     xdn_more       :: Bool,
                     xdn_timestamp  :: Time,
                     xdn_area       :: Rectangle,
                     xdn_geometry   :: Rectangle }


-- for XFree() (already included from Xdamage.h, but I don't know if I can count on that.)
#include <X11/Xlib.h>

#include <X11/extensions/Xdamage.h>

-- | Creates a damage object to monitor changes to Drawable
foreign import ccall "XDamageCreate"
    xdamageCreate :: Display-> Drawable -> DamageReportLevel -> IO(Damage)
        
-- | Destroys damage.
foreign import ccall "XDamageDestroy"
    xdamageDestroy :: Display -> Damage -> IO ()

foreign import ccall "XDamageSubtract"
    xdamageSubtract :: Display -> Damage -> Ptr Region -> Ptr Region -> IO ()

foreign import ccall "XDamageAdd"
    xdamageAdd :: Display -> Drawable -> Region -> IO ()

xdamageQueryExtension :: Display -> IO (Maybe (CInt, CInt))
xdamageQueryExtension dpy = wrapPtr2 (cXdamageQueryExtension dpy) go
  where go False _ _                = Nothing
        go True eventbase errorbase = Just (fromIntegral eventbase, fromIntegral errorbase)

xdamageQueryVersion :: Display -> IO (Maybe (CInt, CInt))
xdamageQueryVersion dpy = wrapPtr2 (cXdamageQueryVersion dpy) go
  where go False _ _        = Nothing
        go True major minor = Just (fromIntegral major, fromIntegral minor)

foreign import ccall "XDamageQueryExtension"
  cXdamageQueryExtension :: Display -> Ptr CInt -> Ptr CInt -> IO Bool

foreign import ccall "XDamageQueryVersion"
  cXdamageQueryVersion :: Display -> Ptr CInt -> Ptr CInt -> IO Bool

foreign import ccall "XFree"
  cXFree :: Ptr a -> IO CInt

wrapPtr2 :: (Storable a, Storable b) => (Ptr a -> Ptr b -> IO c) -> (c -> a -> b -> d) -> IO d
wrapPtr2 cfun f =
  withPool $ \pool -> do aptr <- pooledMalloc pool
                         bptr <- pooledMalloc pool
                         ret <- cfun aptr bptr
                         a <- peek aptr
                         b <- peek bptr
                         return (f ret a b)

