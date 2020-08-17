#!/bin/sh

set -e

#TODO: change  the repo name!!!
echo "get the releases"
RELEASES=`curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/gitoleg/bap/releases`
echo $RELEASES


# check if the first argument in the list (the most recent one) is
# a prerelease
echo "get the latest prerelease flag"
FIRST=`echo $RELEASES | jq '.[0]'`
echo first $FIRST

IsPRERELEASE=`echo $RELEASES | jq '.[0].prerelease'`

echo "check is it's true"
PRERELEASE=
if [ "check$IsPRERELEASE" = "checktrue" ]; then
    PRERELEASE=`echo $RELEASES | jq '.[0].id'`
fi

echo "::set-output name=id::$PRERELEASE"
