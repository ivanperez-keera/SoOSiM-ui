{-# LANGUAGE CPP #-}
module Paths.CustomPaths
  (module Paths_SoOSiM_ui
#ifndef linux_HOST_OS
  , module Paths.CustomPaths
#endif
  )
 where

import Paths_SoOSiM_ui

#ifndef linux_HOST_OS
vendorKey :: String
vendorKey = ""

programKey :: String
programKey = ""
#endif
