#!/bin/bash

for foo in Super Trial
do
  cp -v /usr/local/bin/autotool-$foo backup/${foo}_Debug.cgi.$(date +"%F_%T")
  cp -v /usr/local/bin/autotool-$foo /var/www/autotool/cgi-bin/${foo}_Debug.cgi
done
