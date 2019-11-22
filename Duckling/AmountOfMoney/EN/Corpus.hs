-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.EN.Corpus
  ( corpus
  , negativeCorpus
  , latentCorpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Resolve (Options(..))
import Duckling.Testing.Types

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "exactly dollars"
      ]

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

latentCorpus :: Corpus
latentCorpus = (testContext, testOptions {withLatent = True}, xs)
  where
    xs = concat
      [ examples (simple Unnamed 5)
                 [ "five"
                 , "5"
                 , "about 5"
                 ]
      , examples (simple Unnamed 7.2)
                 [ "7.2"
                 , "7.20000"
                 ]
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 1)
             [ "$1"
             , "one dollar"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "$ 10"
             , "10$"
             , "10 dollars"
             , "ten dollars"
             ]
  , examples (simple Cent 10)
             [ "10 cent"
             , "ten pennies"
             , "ten cents"
             , "10¢"
             ]
  , examples (simple Dollar 1e4)
             [ "$10K"
             , "10k$"
             , "$10,000"
             ]
  , examples (simple USD 3.14)
             [ "USD3.14"
             , "3.14US$"
             , "US$ 3.14"
             , "US$3 and fourteen"
             ]
  , examples (simple EUR 20)
             [ "20\x20ac"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             , "EUR 20.0"
             , "20€"
             , "20 €ur"
             ]
  , examples (simple Pound 10)
             [ "\x00a3\&10"
             , "ten pounds"
             ]
  , examples (simple INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20 Rupees"
             , "20Rs"
             , "Rs20"
             ]
  , examples (simple INR 20.43)
             [ "twenty rupees 43 cents"
             ]
  , examples (simple Dollar 20.43)
             [ "20 dollar 43 cents"
             , "20 dollars 43 cents"
             , "twenty dollar 43 cents"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             , "3 GBP 1 pence"
             , "3 GBP and one"
             ]
  , examples (simple CAD 3.03)
             [ "CAD3.03"
             , "CAD 3.03"
             , "3 CAD 3 cents"
             ]
  , examples (simple CHF 3.04)
             [ "CHF3.04"
             , "CHF 3.04"
             , "3 CHF 4 cents"
             ]
  , examples (simple CNY 3)
             [ "CNY3"
             , "CNY 3"
             , "3 CNY"
             , "3 yuan"
             ]
  , examples (simple Unnamed 42)
             [ "42 bucks"
             , "around 42 bucks"
             , "exactly 42 bucks"
             ]
  , examples (simple KWD 42)
             [ "42 KWD"
             , "42 kuwaiti Dinar"
             ]
  , examples (simple LBP 42)
             [ "42 LBP"
             , "42 Lebanese Pounds"
             ]
  , examples (simple EGP 42)
             [ "42 EGP"
             , "42 egyptianpound"
             ]
  , examples (simple QAR 42)
             [ "42 QAR"
             , "42 qatari riyals"
             ]
  , examples (simple SAR 42)
             [ "42 SAR"
             , "42 Saudiriyal"
             ]
  , examples (simple BGN 42)
             [ "42 BGN"
             ]
  , examples (simple MYR 42)
             [ "42 MYR"
             , "42 RM"
             , "RM 42"
             , "MYR 42"
             , "42MYR"
             , "42RM"
             , "RM42"
             , "MYR42"
             , "ringgit 42"
             , "42 ringgit"
             , "42 malaysia ringgit"
             , "malaysia ringgit 42"
             , "42 malaysian ringgit"
             , "malaysian ringgit 42"
             , "42 malaysia ringgits"
             , "malaysia ringgits 42"
             , "42 malaysian ringgits"
             , "malaysian ringgits 42"
             ]
  , examples (simple MYR 20.43)
             [ "20 ringgit and 43 sen"
             , "twenty ringgit 43 sens"
             ]
  , examples (simple Dinar 10)
             [ "10 dinars"
             ]
  , examples (simple ILS 10)
             [ "ten shekels"
             , "10 ILS"
             ]
  , examples (simple Riyal 10)
             [ "ten riyals"
             , "10 riyals"
             ]
  , examples (simple Rial 10)
             [ "ten rials"
             , "10 rials"
             ]
   , examples (simple INR 5e5)
              [ "5 lakh rupees"
              , "five lakhs rupees"
              ]
   , examples (simple INR 4e7)
              [ "four crore rupees"
              , "4 crores rupees"
              ]
   , examples (simple MNT 10)
              [ "ten tugriks"
              , "10 Tugrik"
              , "10MNT"
              ]
   , examples (simple USD 4.7e9)
              [ "US$4.7 billion"
              , "US$ 4.7 billion"
              ]
  ]
