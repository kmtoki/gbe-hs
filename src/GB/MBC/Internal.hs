module GB.MBC.Internal (MBC(..)) where

import GB.Cartridge
import GB.Utils

data MBC = MBC {
  regs :: Store Word64,
  cartridge :: Cartridge,
  rom :: ROM,
  ram :: RAM,
  ramEx ::RAM
  }

