module Skype.LogParser (
    LogParser(parseSkypeLog)
) where

import Skype.Entry

class LogParser s where
    parseSkypeLog :: s -> [SkypeEntry]

