-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.DE.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale DE Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple EUR 20)
             [ "20€"
             , "€20"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             ]
  , examples (simple EUR 29.99)
             [ "EUR29,99"
             ]
  , examples (simple EUR 7000000)
             [ "€7 Millionen"
             ]
  , examples (simple EUR 10000000)
             [ "EUR 10 Mio"
             ]
  , examples (simple EUR 100000)
             [ "100T€"
             ]
  , examples (simple EUR 50000)
             [ "50T€"
             , "€50T"
             ]
  , examples (simple EUR 67318205.45)
             [ "67.318.205,45 €"
             ]
  , examples (simple EUR 8199999.999999999)
             [ "EUR 8,2 Millionen"
             , "€ 8,2 Millionen"
             , "€8,2 Millionen"
             ]
  ]
