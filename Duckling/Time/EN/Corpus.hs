-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.Corpus
  ( corpus
  , defaultCorpus
  , negativeCorpus
  , latentCorpus
  ) where

import Data.String
import Prelude

import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

defaultCorpus :: Corpus
defaultCorpus = (testContext, testOptions, allExamples ++ custom)
  where
    custom = concat
      [ examples (datetime (2013, 2, 15, 0, 0, 0) Day)
                 [ "2/15"
                 , "on 2/15"
                 , "2 / 15"
                 , "2-15"
                 , "2 - 15"
                 ]
      , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
                 [ "10/31/1974"
                 , "10/31/74"
                 , "10-31-74"
                 , "10.31.1974"
                 ]
      , examples (datetime (2013, 4, 25, 16, 0, 0) Minute)
                 [ "4/25 4:00pm"
                 ]
      , examples (datetimeHoliday (2013, 11, 28, 0, 0, 0) Day "Thanksgiving Day")
                 [ "thanksgiving day"
                 , "thanksgiving"
                 , "thanksgiving 2013"
                 , "this thanksgiving"
                 , "next thanksgiving day"
                 , "thanksgiving in 9 months"
                 , "thanksgiving 9 months from now"
                 ]
      , examples (datetimeHoliday (2014, 11, 27, 0, 0, 0) Day "Thanksgiving Day")
                 [ "thanksgiving of next year"
                 , "thanksgiving in a year"
                 , "thanksgiving 2014"
                 ]
      , examples (datetimeHoliday (2012, 11, 22, 0, 0, 0) Day "Thanksgiving Day")
                 [ "last thanksgiving"
                 , "thanksgiving day 2012"
                 , "thanksgiving 3 months ago"
                 , "thanksgiving 1 year ago"
                 ]
      , examples (datetimeHoliday (2016, 11, 24, 0, 0, 0) Day "Thanksgiving Day")
                 [ "thanksgiving 2016"
                 , "thanksgiving in 3 years"
                 ]
      , examples (datetimeHoliday (2017, 11, 23, 0, 0, 0) Day "Thanksgiving Day")
                 [ "thanksgiving 2017"
                 ]
      ]

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "laughing out loud"
      , "1 adult"
      , "we are separated"
      , "25"
      , "this is the one"
      , "this one"
      , "this past one"
      , "at single"
      , "at a couple of"
      , "at pairs"
      , "at a few"
      , "at dozens"
      , "single o'clock"
      , "dozens o'clock"
      , "Rat 6"
      , "rat 6"
      , "3 30"
      , "three twenty"
      , "at 650.650.6500"
      , "at 650-650-6500"
      , "two sixty a m"
      , "Pay ABC 2000"
      , "4a"
      , "4a."
      , "A4 A5"
      ]

latentCorpus :: Corpus
latentCorpus = (testContext, testOptions {withLatent = True}, xs)
  where
    xs = concat
      [ examples (datetime (2013, 2, 24, 0, 0, 0) Day)
                 [ "the 24"
                 , "On 24th"
                 ]
      , examples (datetime (2013, 2, 12, 7, 0, 0) Hour)
                 [ "7"
                 , "7a"
                 ]
      , examples (datetime (2013, 2, 12, 19, 0, 0) Hour)
                 [ "7p"
                 ]
      --, examples (datetime (1954, 1, 1, 0, 0, 0) Year)
      --           [ "1954"
      --           ]
      , examples (datetime (2013, 5, 1, 0, 0, 0) Month)
                 [ "May"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
                 [ "morning"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
                 [ "afternoon"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
                 [ "evening"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
                 [ "night"
                 ]
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "now"
             , "right now"
             , "just now"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "today"
             , "at this time"
             ]
  , examples (datetime (2013, 2, 1, 0, 0, 0) Day)
             [ "2/2013"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "in 2014"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "yesterday"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "tomorrow"
             , "tomorrows"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "monday"
             , "mon."
             , "this monday"
             , "Monday, Feb 18"
             , "Mon, February 18"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "tuesday"
             , "Tuesday the 19th"
             , "Tuesday 19th"
             ]
  , examples (datetime (2013, 8, 15, 0, 0, 0) Day)
             [ "Thu 15th"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "thursday"
             , "thu"
             , "thu."
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "friday"
             , "fri"
             , "fri."
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "saturday"
             , "sat"
             , "sat."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "sunday"
             , "sun"
             , "sun."
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "the 1st of march"
             , "first of march"
             , "march first"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "march 3"
             ]
  , examples (datetime (2013, 3, 15, 0, 0, 0) Day)
             [ "the ides of march"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "march 3 2015"
             , "march 3rd 2015"
             , "march third 2015"
             , "3/3/2015"
             , "3/3/15"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "on the 15th"
             , "the 15th of february"
             , "15 of february"
             , "february the 15th"
             , "february 15"
             , "15th february"
             , "February 15"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "Aug 8"
             ]
  , examples (datetime (2014, 3, 1, 0, 0, 0) Month)
             [ "March in 1 year"
             , "March in a year"
             ]
  , examples (datetime (2014, 7, 18, 0, 0, 0) Day)
             [ "Fri, Jul 18"
             , "Jul 18, Fri"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Month)
             [ "October 2014"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14april 2015"
             , "April 14, 2015"
             , "14th April 15"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "next tuesday"
             , "around next tuesday"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "friday after next"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "next March"
             ]
  , examples (datetime (2014, 3, 1, 0, 0, 0) Month)
             [ "March after next"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "Sunday, Feb 10"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Wed, Feb13"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "this week"
             , "current week"
             , "coming week"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "last week"
             , "past week"
             , "previous week"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "next week"
             , "the following week"
             , "around next week"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
             [ "last month"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "next month"
             ]
  , examples (datetime (2013, 3, 20, 0, 0, 0) Day)
             [ "20 of next month"
             , "20th of the next month"
             , "20th day of next month"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "20th of the current month"
             , "20 of this month"
             ]
  , examples (datetime (2013, 1, 20, 0, 0, 0) Day)
             [ "20th of the previous month"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "this quarter"
             , "this qtr"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "next quarter"
             , "next qtr"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "third quarter"
             , "3rd quarter"
             , "third qtr"
             , "3rd qtr"
             , "the 3rd qtr"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "4th quarter 2018"
             , "4th qtr 2018"
             , "the 4th qtr of 2018"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "last year"
             , "last yr"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Year)
             [ "this year"
             , "current year"
             , "this yr"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "next year"
             , "next yr"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "in 2014 A.D.",
               "in 2014 AD"
             ]
  , examples (datetime (-2014, 1, 1, 0, 0, 0) Year)
             [ "in 2014 B.C.",
               "in 2014 BC"
             ]
  , examples (datetime (14, 1, 1, 0, 0, 0) Year)
             [ "in 14 a.d."
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "last sunday"
             , "sunday from last week"
             , "last week's sunday"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "last tuesday"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "next tuesday"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "next wednesday"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "wednesday of next week"
             , "wednesday next week"
             , "wednesday after next"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "friday after next"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "monday of this week"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "tuesday of this week"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "wednesday of this week"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "the day after tomorrow"
             ]
  , examples (datetime (2013, 2, 14, 17, 0, 0) Hour)
             [ "day after tomorrow 5pm"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "the day before yesterday"
             ]
  , examples (datetime (2013, 2, 10, 8, 0, 0) Hour)
             [ "day before yesterday 8am"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "last Monday of March"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "last Sunday of March 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "third day of october"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "first week of october 2014"
             ]
  , examples (datetime (2018, 12, 10, 0, 0, 0) Week)
             [ "third last week of 2018"
             , "the third last week of 2018"
             , "the 3rd last week of 2018"
             ]
  , examples (datetime (2018, 10, 15, 0, 0, 0) Week)
             [ "2nd last week of October 2018"
             , "the second last week of October 2018"
             ]
  , examples (datetime (2013, 5, 27, 0, 0, 0) Day)
             [ "fifth last day of May"
             , "the 5th last day of May"
             ]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Week)
             [ "the week of october 6th"
             ]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Week)
             [ "the week of october 7th"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "last day of october 2015"
             , "last day in october 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "last week of september 2014"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "first tuesday of october"
             , "first tuesday in october"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "third tuesday of september 2014"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "first wednesday of october 2014"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "second wednesday of october 2014"
             ]
  , examples (datetime (2015, 1, 13, 0, 0, 0) Day)
             [ "third tuesday after christmas 2014"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "3am"
             , "3 in the AM"
             , "3 AM"
             , "3 oclock am"
             , "three am"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18am"
             , "3:18a"
             ]
  , examples (datetime (2016, 2, 1, 7, 0, 0) Hour)
             [ "7am in 3 years"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "3pm"
             , "3PM"
             , "3pm"
             , "3 oclock pm"
             , "3 o'clock in the afternoon"
             , "3ish pm"
             , "3pm approximately"
             , "about 3pm"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "15 past 3pm"
             , "a quarter past 3pm"
             , "3:15 in the afternoon"
             , "15:15"
             , "3:15pm"
             , "3:15PM"
             , "3:15p"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "20 past 3pm"
             , "3:20 in the afternoon"
             , "3:20 in afternoon"
             , "twenty after 3pm"
             , "3:20p"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "half past three pm"
             , "half past 3 pm"
             , "15:30"
             , "3:30pm"
             , "3:30PM"
             , "330 p.m."
             , "3:30 p m"
             , "3:30"
             , "half three"
             ]
  , examples (datetime (2013, 2, 12, 9, 59, 0) Minute)
             [ "nine fifty nine a m"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ "15:23:24"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "a quarter to noon"
             , "11:45am"
             , "15 to noon"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8 tonight"
             , "eight tonight"
             , "8 this evening"
             , "8 in the evening"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "7:30 PM on Fri, Sep 20"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "9am on Saturday"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "on Saturday for 9am"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Minute)
             [ "Fri, Jul 18, 2014 07:00 PM"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "in a sec"
             , "one second from now"
             , "in 1\""
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "in a minute"
             , "in one minute"
             , "in 1'"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "in 2 minutes"
             , "in 2 more minutes"
             , "2 minutes from now"
             , "in a couple of minutes"
             , "in a pair of minutes"
             ]
  , examples (datetime (2013, 2, 12, 4, 33, 0) Second)
             [ "in three minutes"
             , "in a few minutes"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "in 60 minutes"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "in a quarter of an hour"
             , "in 1/4h"
             , "in 1/4 h"
             , "in 1/4 hour"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "in half an hour"
             , "in 1/2h"
             , "in 1/2 h"
             , "in 1/2 hour"
             ]
  , examples (datetime (2013, 2, 12, 5, 15, 0) Second)
             [ "in three-quarters of an hour"
             , "in 3/4h"
             , "in 3/4 h"
             , "in 3/4 hour"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ "in 2.5 hours"
             , "in 2 and an half hours"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "in one hour"
             , "in 1h"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "in a couple hours"
             , "in a couple of hours"
             ]
  , examples (datetime (2013, 2, 12, 7, 30, 0) Minute)
             [ "in a few hours"
             , "in few hours"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "in 24 hours"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "in a day"
             , "a day from now"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Second)
             [ "a day from right now"
             ]
  , examples (datetime (2016, 2, 12, 0, 0, 0) Day)
             [ "3 years from today"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "in 7 days"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "in 1 week"
             , "in a week"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "in about half an hour"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 days ago"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14 days Ago"
             , "a fortnight ago"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "a week ago"
             , "one week ago"
             , "1 week ago"
             ]
  , examples (datetime (2013, 1, 31, 0, 0, 0) Day)
             [ "2 thursdays back"
             , "2 thursdays ago"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "three weeks ago"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "three months ago"
             ]
  , examples (datetime (2011, 2, 1, 0, 0, 0) Month)
             [ "two years ago"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "7 days hence"
             ]
  , examples (datetime (2013, 2, 26, 4, 0, 0) Hour)
             [ "14 days hence"
             , "a fortnight hence"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "a week hence"
             , "one week hence"
             , "1 week hence"
             ]
  , examples (datetime (2013, 3, 5, 0, 0, 0) Day)
             [ "three weeks hence"
             ]
  , examples (datetime (2013, 5, 12, 0, 0, 0) Day)
             [ "three months hence"
             ]
  , examples (datetime (2015, 2, 1, 0, 0, 0) Month)
             [ "two years hence"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "one year After christmas"
             , "a year from Christmas"
             ]
  , examples (datetimeHoliday (2013, 12, 25, 0, 0, 0) Day "Christmas")
             [ "xmas"
             , "christmas"
             , "christmas day"
             ]
  , examples (datetimeHoliday (2013, 12, 25, 18, 0, 0) Hour "Christmas")
             [ "xmas 6 pm"
             ]
  , examples (datetimeHoliday (2013, 12, 31, 0, 0, 0) Day "New Year's Eve")
             [ "new year's eve"
             , "new years eve"
             ]
  , examples (datetimeHoliday (2014, 1, 1, 0, 0, 0) Day "New Year's Day")
             [ "new year's day"
             , "new years day"
             ]
  , examples (datetimeHoliday (2013, 2, 14, 0, 0, 0) Day "Valentine's Day")
             [ "valentine's day"
             , "valentine day"
             ]
  , examples (datetime (2013, 7, 4, 0, 0, 0) Day)
             [ "4th of July"
             , "4 of july"
             ]
  , examples (datetimeHoliday (2013, 10, 31, 0, 0, 0) Day "Halloween")
             [ "halloween"
             , "next halloween"
             , "Halloween 2013"
             ]
  , examples (datetimeHoliday (2013, 11, 29, 0, 0, 0) Day "Black Friday")
             [ "black friday"
             , "black friday of this year"
             , "black friday 2013"
             ]
  , examples (datetimeHoliday (2017, 11, 24, 0, 0, 0) Day "Black Friday")
             [ "black friday 2017"
             ]
  , examples (datetimeHoliday (2013, 10, 16, 0, 0, 0) Day "Boss's Day")
             [ "boss's day"
             , "boss's"
             , "boss day"
             , "next boss's day"
             ]
  , examples (datetimeHoliday (2016, 10, 17, 0, 0, 0) Day "Boss's Day")
             [ "boss's day 2016"
             ]
  , examples (datetimeHoliday (2021, 10, 15, 0, 0, 0) Day "Boss's Day")
             [ "boss's day 2021"
             ]
  , examples (datetimeHoliday (2014, 1, 20, 0, 0, 0) Day "Martin Luther King's Day")
             [ "MLK day"
             , "next Martin Luther King day"
             , "this MLK day"
             ]
  , examples (datetimeHoliday (2013, 1, 21, 0, 0, 0) Day "Martin Luther King's Day")
             [ "last MLK Jr. day"
             , "MLK day 2013"
             ]
  , examples (datetimeHoliday (2012, 1, 16, 0, 0, 0) Day "Martin Luther King's Day")
             [ "MLK day of last year"
             , "MLK day 2012"
             , "Civil Rights Day of last year"
             ]
  , examples (datetimeHoliday (2013, 11, 1, 0, 0, 0) Day "World Vegan Day")
             [ "world vegan day"
             ]
  , examples (datetimeHoliday (2013, 3, 31, 0, 0, 0) Day "Easter Sunday")
             [ "easter"
             , "easter 2013"
             ]
 , examples (datetimeHoliday (2012, 4, 08, 0, 0, 0) Day "Easter Sunday")
             [ "last easter"
             ]
  , examples (datetimeHoliday (2013, 4, 1, 0, 0, 0) Day "Easter Monday")
             [ "easter mon"
             ]
  , examples (datetimeHoliday (2010, 4, 4, 0, 0, 0) Day "Easter Sunday")
             [ "easter 2010"
             , "Easter Sunday two thousand ten"
             ]
  , examples (datetime (2013, 4, 3, 0, 0, 0) Day)
             [ "three days after Easter"
             ]
  , examples (datetimeHoliday (2013, 3, 28, 0, 0, 0) Day "Maundy Thursday")
             [ "Maundy Thursday"
             , "Covenant thu"
             , "Thu of Mysteries"
             ]
  , examples (datetimeHoliday (2013, 5, 19, 0, 0, 0) Day "Pentecost")
             [ "Pentecost"
             , "white sunday 2013"
             ]
  , examples (datetimeHoliday (2013, 5, 20, 0, 0, 0) Day "Whit Monday")
             [ "whit monday"
             , "Monday of the Holy Spirit"
             ]
  , examples (datetimeHoliday (2013, 3, 24, 0, 0, 0) Day "Palm Sunday")
             [ "palm sunday"
             , "branch sunday 2013"
             ]
  , examples (datetimeHoliday (2013, 5, 26, 0, 0, 0) Day "Trinity Sunday")
             [ "trinity sunday"
             ]
  , examples (datetimeHoliday (2013, 2, 12, 0, 0, 0) Day "Shrove Tuesday")
             [ "pancake day 2013"
             ]
  , examples (datetimeHoliday (2018, 4, 8, 0, 0, 0) Day "Orthodox Easter Sunday")
             [ "orthodox easter 2018"
             ]
  , examples (datetimeHoliday (2018, 2, 19, 0, 0, 0) Day "Clean Monday")
             [ "clean monday 2018"
             , "orthodox shrove monday two thousand eighteen"
             ]
  , examples (datetimeHoliday (2018, 3, 31, 0, 0, 0) Day "Lazarus Saturday")
             [ "lazarus saturday 2018"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "4pm CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "Thursday 8:00 GMT"
             , "Thursday 8:00 gmt"
             ]
  , examples (datetime (2013, 2, 14, 14, 0, 0) Minute)
             [ "Thursday 8:00 PST"
             , "Thursday 8:00 pst"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "today 2pm"
             , "2pm"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "3pm tomorrow"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "1:30pm"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "in 15 minutes"
             , "in 15'"
             , "in 15"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             , "approximately 1030"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "next monday"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "12pm"
             , "noon"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "12am"
             , "midnight"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "March"
             , "in March"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "5pm tomorrow"
             , "tomorrow 5pm"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "on the first"
             , "the 1st"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "around 1030"
             , "ten thirty am"
             ]
  , examples (datetime (2013, 2, 12, 19, 30, 0) Minute)
             [ "730 in the evening"
             , "seven thirty p.m."
             ]
  , examples (datetime (2013, 2, 12, 4, 23, 0) Minute)
    -- yes, the result is in the past, we may need to revisit
             [ "4:23"
             , "4:23am"
             , "four twenty-three a m"
             ]
  , examples (datetimeHoliday (2014, 1, 31, 0, 0, 0) Day "Chinese New Year")
             [ "chinese new year"
             , "chinese lunar new year's day"
             ]
  , examples (datetimeHoliday (2013, 2, 10, 0, 0, 0) Day "Chinese New Year")
             [ "last chinese new year"
             , "last chinese lunar new year's day"
             ]
  , examples (datetimeHoliday (2018, 2, 16, 0, 0, 0) Day "Chinese New Year")
             [ "chinese new year's day 2018"
             ]
  , examples (datetimeHoliday (2018, 9, 18, 0, 0, 0) Day "Yom Kippur")
             [ "yom kippur 2018"
             ]
  , examples (datetimeHoliday (2018, 9, 30, 0, 0, 0) Day "Shemini Atzeret")
             [ "shemini atzeret 2018"
             ]
  , examples (datetimeHoliday (2018, 10, 1, 0, 0, 0) Day "Simchat Torah")
             [ "simchat torah 2018"
             ]
  , examples (datetimeHoliday (2018, 7, 21, 0, 0, 0) Day "Tisha B'Av")
             [ "tisha b'av 2018"
             ]
  , examples (datetimeHoliday (2018, 4, 18, 0, 0, 0) Day "Yom Ha'atzmaut")
             [ "yom haatzmaut 2018"
             ]
  , examples (datetimeHoliday (2017, 5, 13, 0, 0, 0) Day "Lag BaOmer")
             [ "lag b'omer 2017"
             ]
  , examples (datetimeHoliday (2018, 4, 11, 0, 0, 0) Day "Yom HaShoah")
             [ "Yom Hashoah 2018"
             , "Holocaust Day 2018"
             ]
  , examples (datetimeHoliday (2017, 11, 30, 0, 0, 0) Day "Mawlid")
             [ "mawlid al-nabawi 2017"
             ]
  , examples (datetimeHoliday (2018, 6, 15, 0, 0, 0) Day "Eid al-Fitr")
             [ "Eid al-Fitr 2018"
             ]
  , examples (datetimeHoliday (2018, 8, 21, 0, 0, 0) Day "Eid al-Adha")
             [ "Eid al-Adha 2018"
             , "id ul-adha 2018"
             , "sacrifice feast 2018"
             , "Bakr Id 2018"
             ]
  , examples (datetimeHoliday (2017, 6, 22, 0, 0, 0) Day "Laylat al-Qadr")
             [ "laylat al kadr 2017"
             , "night of measures 2017"
             ]
  , examples (datetimeHoliday (2018, 6, 11, 0, 0, 0) Day "Laylat al-Qadr")
             [ "laylat al-qadr 2018"
             , "night of power 2018"
             ]
  , examples (datetimeHoliday (2018, 9, 11, 0, 0, 0) Day "Islamic New Year")
             [ "Islamic New Year 2018"
             , "Amun Jadid 2018"
             ]
  , examples (datetimeHoliday (2017, 9, 30, 0, 0, 0) Day "Ashura")
             [ "day of Ashura 2017"
             ]
  , examples (datetimeHoliday (2018, 1, 30, 0, 0, 0) Day "Tu BiShvat")
             [ "tu bishvat 2018"
             ]
  , examples (datetimeHoliday (2017, 6, 23, 0, 0, 0) Day "Jumu'atul-Wida")
             [ "Jamat Ul-Vida 2017"
             , "Jumu'atul-Wida 2017"
             ]
  , examples (datetimeHoliday (2018, 6, 8, 0, 0, 0) Day "Jumu'atul-Wida")
             [ "Jamat Ul-Vida 2018"
             , "Jumu'atul-Wida 2018"
             ]
  , examples (datetimeHoliday (2018, 4, 13, 0, 0, 0) Day "Isra and Mi'raj")
             [ "isra and mi'raj 2018"
             , "the prophet's ascension 2018"
             ]
  , examples (datetimeHoliday (2019, 4, 3, 0, 0, 0) Day "Isra and Mi'raj")
             [ "the night journey 2019"
             , "ascension to heaven 2019"
             ]
  , examples (datetimeHoliday (2017, 10, 17, 0, 0, 0) Day "Dhanteras")
             [ "dhanatrayodashi in 2017"
             ]
  , examples (datetimeHoliday (2019, 10, 25, 0, 0, 0) Day "Dhanteras")
             [ "dhanteras 2019"
             ]
  , examples (datetimeHoliday (2019, 10, 26, 0, 0, 0) Day "Naraka Chaturdashi")
             [ "kali chaudas 2019"
             , "choti diwali two thousand nineteen"
             ]
  , examples (datetimeHoliday (2019, 10, 27, 0, 0, 0) Day "Diwali")
             [ "diwali 2019"
             , "Deepavali in 2019"
             , "Lakshmi Puja six years hence"
             ]
  , examples (datetimeHoliday (2019, 10, 29, 0, 0, 0) Day "Bhai Dooj")
             [ "bhai dooj 2019"
             ]
  , examples (datetimeHoliday (2019, 11, 2, 0, 0, 0) Day "Chhath")
             [ "chhath 2019"
             , "dala puja 2019"
             , "Surya Shashthi in 2019"
             ]
  , examples (datetimeHoliday (2021, 10, 12, 0, 0, 0) Day "Maha Saptami")
             [ "Maha Saptami 2021"
             ]
  , examples (datetimeHoliday (2018, 10, 18, 0, 0, 0) Day "Vijayadashami")
             [ "Dussehra 2018"
             , "vijayadashami in five years"
             ]
  , examples (datetimeHoliday (2018, 8, 26, 0, 0, 0) Day "Raksha Bandhan")
             [ "rakhi 2018"
             ]
  , examples (datetimeHoliday (2018, 1, 14, 0, 0, 0) Day "Thai Pongal")
             [ "pongal 2018"
             , "makara sankranthi 2018"
             ]
  , examples (datetimeHoliday (2018, 1, 13, 0, 0, 0) Day "Boghi")
             [ "bogi pandigai 2018"
             ]
  , examples (datetimeHoliday (2018, 1, 15, 0, 0, 0) Day "Mattu Pongal")
             [ "maattu pongal 2018"
             ]
  , examples (datetimeHoliday (2018, 1, 16, 0, 0, 0) Day "Kaanum Pongal")
             [ "kaanum pongal 2018"
             , "kanni pongal 2018"
             ]
  , examples (datetimeHoliday (2019, 1, 15, 0, 0, 0) Day "Thai Pongal")
             [ "makar sankranti 2019"
             , "maghi in 2019"
             ]
  , examples (datetimeHoliday (2018, 8, 24, 0, 0, 0) Day "Thiru Onam")
             [ "onam 2018"
             , "Thiru Onam 2018"
             , "Thiruvonam 2018"
             ]
  , examples (datetimeHoliday (2019, 2, 10, 0, 0, 0) Day "Vasant Panchami")
             [ "vasant panchami in 2019"
             , "basant panchami 2019"
             ]
  , examples (datetimeHoliday (2019, 3, 20, 0, 0, 0) Day "Holika Dahan")
             [ "chhoti holi 2019"
             , "holika dahan 2019"
             , "kamudu pyre 2019"
             ]
  , examples (datetimeHoliday (2019, 3, 21, 0, 0, 0) Day "Holi")
             [ "holi 2019"
             , "dhulandi 2019"
             , "phagwah 2019"
             ]
  , examples (datetimeHoliday (2013, 5, 24, 0, 0, 0) Day "Vesak")
             [ "vesak"
             , "vaisakha"
             , "Buddha day"
             , "Buddha Purnima"
             ]
  ]
