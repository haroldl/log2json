{-
    Main.hs
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
module Main where

import Text.LogFormat

import Data.Map ( toList )
import System.Environment ( getArgs )
import System.IO
import Text.JSON
import Text.Parsec

main = do args <- getArgs
          logFormat <- if length args < 1
                         then do putStrLn "Enter your LogFormat string:"
                                 getLine
                         else return (head args)
          parser <- compileLogFormat logFormat
          let multiParser = sepEndBy parser newline
          if length args < 2
             then processFileHandle multiParser "stdin" stdin
             else do sequence $ map (processFile multiParser) (tail args)
                     return ()

processFile parser filename = withFile filename ReadMode (processFileHandle parser filename)

processFileHandle parser filename fh =
  do contents <- hGetContents fh
     case (parse parser name contents) of
       Left parseErr ->
         fail $ invalidThingMessage "log file" filename parseErr
       Right maps ->
         let json = encode $ map (toJSObject . toList) maps in
           putStrLn json
  where name = "LogFormat parser for file " ++ filename

-- Parse the LogFormat string to get a log record parser.
compileLogFormat logFormat =
  case (logFormatParser logFormat) of
     Left parseErr ->
       fail $ invalidThingMessage "LogFormat string" logFormat parseErr
     Right parser ->
       return parser

-- Ask for a log record, parse it, and print the resulting JSON object.
parseALogRecord logFormat parser inputLine =
  case (parse parser ("LogFormat Tool [" ++ logFormat ++ "]") inputLine) of
    Left parseErr ->
      fail $ invalidThingMessage "log record" inputLine parseErr
    Right map ->
      putStrLn $ encode $ toJSObject $ toList map

invalidThingMessage thing input parseError =
  "Invalid " ++ thing ++ " \"" ++ input ++ "\": " ++ (show parseError)
