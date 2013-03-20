-- -*- mode: haskell -*-

module Local where

debug :: Bool
debug = False

super_cgi_name :: String
super_cgi_name = 
      if debug then "Super_Debug.cgi" else "Super.cgi"

trial_cgi_name :: String
trial_cgi_name = 
      if debug then "Trial_Debug.cgi" else "Trial.cgi"
