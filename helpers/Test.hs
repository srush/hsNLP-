module Helpers.Test (
                      module Test.QuickCheck,
                      module Test.Framework.Providers.QuickCheck2 ,
                      module Test.Framework,
                      module Test.HUnit ,
                      module Test.Framework.Providers.HUnit
                     ) where

import Test.QuickCheck
import Test.HUnit hiding (Testable)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
    
