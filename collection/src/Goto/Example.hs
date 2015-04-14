module Goto.Example where

import Goto

student :: Program
student =  [ GotoZ 1 4, Inc 0, Dec 1, Goto 0, Stop ]
