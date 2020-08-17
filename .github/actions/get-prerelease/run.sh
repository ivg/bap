#!/bin/sh

#TODO: change  the repo name!!!

RELEASES=`curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/gitoleg/bap/releases`

# check if the first argument in the list (the most recent one) is
# a prerelease
IsPRERELEASE=`echo $RELEASES | jq '.[0].prerelease'`

PRERELEASE=
if [ "check$IsPRERELEASE" == "checktrue" ]; then
    PRERELEASE=`echo $RELEASES | jq '.[0].id'`
fi

echo "::set-output name=id::$PRERELEASE"
