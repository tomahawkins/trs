module PIC.Configuration
  (
  -- * Types
    Configuration
  -- * Processor Configuration
  , configuration
  , dataWidth
  , instrWidth
  , dataMemoryDepth
  , instrMemoryDepth
  , basicConfig
  ) where

import Utils

-- | Processor configuration.
data Configuration = Configuration Int Int Int

-- | Defines a PIC 'Configuration', given data width, data memory depth, and instruction memory depth.
configuration :: Int -> Int -> Int -> Configuration
configuration dW dMD iMD =
  if dW `mod` 2 == 1
    then error $ "Invalid PIC data width:  Data width (" ++ show dW ++ ") must be even."
    else if dMD > power 2 (dataWidth config - 1) || dMD <= 0
      then error $ "Invalid PIC data memory depth.  Requires:  2 ^ (dataWidth - 1) >= dataMemoryDepth(" ++ show dMD ++ ") > 0."
      else if iMD <= 0
      then error $ "Invalid PIC instruction memory depth.  Requires: instrMemoryDepth(" ++ show iMD ++ ") > 0."
      else config
  where
  config = Configuration dW dMD iMD

-- | Data width of a PIC 'Configuration'
dataWidth :: Configuration -> Int
dataWidth (Configuration dw _ _) = dw

-- | Instruction width of a PIC 'Configuration'.
instrWidth :: Configuration -> Int
instrWidth config = dataWidth config + 6

-- | Data memory depth of a PIC 'Configuration'.
dataMemoryDepth (Configuration _ dMD _) = dMD

-- | Instruction memory depth of a PIC 'Configuration'.
instrMemoryDepth (Configuration _ _ iMD) = iMD

-- | Basic processor configuration: 14-bit instruction width, 1024 deep instruction memory, 8-bit data width.
basicConfig :: Configuration
--basicConfig = configuration 8 128 1024
basicConfig = configuration 16 1024 1024

