{-# LANGUAGE OverloadedStrings #-}
module Hispania.TestTypes where

import           Data.ByteString.Char8 as BS
import           Hispania.Types

-- testFrom = (GenericHeader (BS.pack "From") (BS.pack "<sip:from@from>"))
testFrom = ("From", AddressHeader (SipAddress (Just "FullName") (SipURI False "fromusr" BS.empty "fromhost" (Just 5060) []) []))

testTo = ("To", (GenericHeader "<sip:to@to>"))

testCallId = ("Call-ID", (GenericHeader "235205720357"))

testReq = Request INVITE (RawURI "sip" "user@host" ) defaultProtoVersion [testFrom, testTo, testCallId] "SDP"

topFrom = getTopHeader testReq "From"

topTo :: Maybe Header
topTo = getTopHeader testReq "To"

topCallId = getTopHeader testReq "Call-ID"
