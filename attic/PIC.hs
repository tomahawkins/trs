-- | PIC support for TRS compilation.

module PIC
  (
  -- * Assembly Instruction Types
    module PIC.Assemble
  -- * PIC Compilation
  , module PIC.Compiler
  -- * Processor Configuration
  , module PIC.Configuration
  -- * Output Generation
  , module PIC.Output
  -- * Instruction Set Simulation
  , module PIC.Simulator
  ) where

import PIC.Assemble
import PIC.Compiler
import PIC.Configuration
import PIC.Output
import PIC.Simulator


