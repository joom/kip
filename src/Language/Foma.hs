{-# LANGUAGE ForeignFunctionInterface #-}

module Language.Foma where

import Control.Monad
-- import Foreign.C
-- import Foreign.Ptr
-- import System.IO.Unsafe (unsafePerformIO)

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal

-- | The type for a finite state machine. Wrapper for a pointer.
newtype FSM = FSM (Ptr ())

foreign import ccall unsafe "fomalib.h fsm_read_binary_file"
  fsmReadBinaryFile' :: CString -> IO FSM

-- | The function to read the binary file with.
fsmReadBinaryFile :: FilePath -> IO FSM
fsmReadBinaryFile = newCString >=> fsmReadBinaryFile'

foreign import ccall "morphology.h ups"
  ups' :: FSM -> CString -> IO (Ptr CString)

ups :: FSM -> String -> IO [String]
ups fsm s = do
  cs <- newCString s
  res <- ups' fsm cs
  arr <- peekArray0 nullPtr res
  mapM peekCString arr <* (mapM_ free arr >> free res)
