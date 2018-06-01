#!/bin/bash
#
# I am a script, intended to run from `cron`, which pings a
# configurable URL, and if no suitable response is forthcoming,
# performs a configurable command.
#
# For example, to monitor racket-pkg-website, try
#
# ./ping-service.sh https://localhost:8444/ping 'touch .../signals/.dumpinfo; svc -du ...'

if [ "$#" != "2" ]
then
    echo 'Usage: ping-service.sh <url> <command>'
    echo 'Note that <command> has to be a single string.'
    exit 1
fi

url="$1"
failurecommand="$2"

# curl flags:
#  -f == fail, interrogate the HTTP response status code
#  -s == silent, don't print a progress meter or any other cruft
#  -k == Ignore certificates, where url is an HTTPS URL
#
if curl -f -s -k --max-time 10 "$url" > /dev/null
then
    # Do nothing -- the retrieval was successful
    true
else
    exec sh -c "$failurecommand"
fi
