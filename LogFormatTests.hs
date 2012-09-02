{-
    LogFormatTests.hs
    Copyright (C) 2012 Harold Lee

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module LogFormatTests where

import Text.LogFormat

import Data.Map as M
import Test.HUnit
import Text.Parsec as P

main = runTestTT allTests

allTests = TestList [testU, testLiteral, testBadLit, testUAndLit,
                     testGetMethod, testPostMethod, testRemoteIP, testLocalIP,
                     testBytesCLF, testBytesCLF2, testBytesCLFBad1, testBytesCLFBad2,
                     testThreeGroups,
                     testHeader, testHeaderQuotes, testHeaderAndCo, testAnonymousHeader]

buildParser logFormat = case logFormatParser logFormat of
  Left parseErr -> do assertFailure $ "Failed to compile LogFormat: " ++ show parseErr ; fail ""
  Right parser  -> return parser

failedParseTest testName logFormat inputLine expMessage =
    testName ~: do parser <- buildParser logFormat
                   case parse parser testName inputLine of
                     Left parseErr -> assertEqual testName expMessage (show parseErr)
                     Right _ -> assertFailure $ "Parse succeeded, but was expecting failure: " ++ expMessage

parseRecordTest testName logFormat inputLine expected =
    testName ~: do parser <- buildParser logFormat
                   map <- applyParser parser (inputLine ++ "\n")
                   assertEqual "Checking log record parse result" (M.fromList expected) map
  where applyParser parser inputLine = case parse parser testName inputLine of
          Left parseErr -> do assertFailure $ "Failed to parse sample log line: " ++ show parseErr ; fail ""
          Right map -> return map

testU = parseRecordTest "testU" "%U" "/abc" exp
  where exp = [("path", "/abc")]

testLiteral = parseRecordTest "testLiteral" "hi" "hi" exp
  where exp = []

testBadLit = failedParseTest "testBadLit" "abc" "def" errMessage
  where errMessage = "\"testBadLit\" (line 1, column 1):\nunexpected \"d\"\nexpecting \"abc\""

testUAndLit = parseRecordTest "testUAndLit" "%U?a=1" "/path/to/somewhere?a=1" exp
  where exp = [("path", "/path/to/somewhere")]

testGetMethod = parseRecordTest "testGetMethod" "method%m" "methodGET" exp
  where exp = [("method", "GET")]

testPostMethod = parseRecordTest "testPostMethod" "method%m" "methodPOST" exp
  where exp = [("method", "POST")]

testRemoteIP = parseRecordTest "testRemoteIP" "%a" "123.45.67.89" exp
  where exp = [("remoteIP", "123.45.67.89")]

testLocalIP = parseRecordTest "testLocalIP" "%A" "123.45.67.89" exp
  where exp = [("localIP", "123.45.67.89")]

testBytesCLF = parseRecordTest "testBytesCLF" "%b" "-" exp
  where exp = [("bytesCLF", "-")]

testBytesCLF2 = parseRecordTest "testBytesCLF2" "%b" "1234" exp
  where exp = [("bytesCLF", "1234")]

testBytesCLFBad1 = failedParseTest "testBytesCLFBad1" "%b" "1,234" errMessage
  where errMessage = "\"testBytesCLFBad1\" (line 1, column 2):\nunexpected \",\"\nexpecting digit or new-line"

testBytesCLFBad2 = failedParseTest "testBytesCLFBad2" "%b" "abc" errMessage
  where errMessage = "\"testBytesCLFBad2\" (line 1, column 1):\nunexpected \"a\"\nexpecting digit or \"-\""

testThreeGroups = parseRecordTest "testThreeGroups" "%%%b%%%s%%%>s" "%123%abc%def" exp
  where exp = [("statusOriginal", "abc"), ("statusLast", "def"), ("bytesCLF", "123")]

testHeader = parseRecordTest "testHeader"  "%{Content-Type}i" "hello world" exp
  where exp = [("header:Content-Type", "hello world")]

testHeaderQuotes = parseRecordTest "testHeaderQuotes" "'%{foo}i'" "'''" exp
  where exp = [("header:foo", "'")]

testHeaderAndCo = parseRecordTest "testHeaderAndCo"  "%%%b'%{Content-Type}i'%B%%" "%123'hello world'456%" exp
  where exp = [("bytesCLF", "123"), ("header:Content-Type", "hello world"), ("bytes", "456")]

testAnonymousHeader = parseRecordTest "testAnonymousHeader" "%i" "hello" exp
  where exp = [("header", "hello")]

-- TODO : test these log formats

-- Common Log Format with Virtual Host
commonLogFormat = "%v %h %l %u %t \"%r\" %>s %b"

-- NCSA extended/combined log format
ncsaLogFormat = "%h %l %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-agent}i\""

defaultLogFormat = "%h %l %u %t \"%r\" %>s %b"
